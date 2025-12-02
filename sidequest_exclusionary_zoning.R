
library(dplyr)
library(readr)
library(sf)
library(terra)
# devtools::install_github("statnmap/HatchedPolygons")
library(HatchedPolygons)


source("supporting_functions.R")

####################
# LOAD BLOCK DATA
####################
# Load raw block data:
# 1950 and 1960 block data from here: https://blogs.gwu.edu/centerforwashingtonareastudies/resources/. Compiled by Leah Brooks and team.
b40_shp <- sf::st_read("block_shapes/1940_block_shapefiles/1940_census_DC_joined.shp", quiet=T)
b50_shp <- sf::st_read("block_shapes/1950_block_shapefiles/1950_census_blocks_joined.shp", quiet=T)
b60_shp <- sf::st_read("block_shapes/1960_block_shapefiles/1960_census_blocks_joined.shp", quiet=T)
# other block data come from IPUMS NHGIS
b70_shp <- sf::st_read("block_shapes/DC_block_1970/DC_block_1970.shp", quiet=T)
b80_shp <- sf::st_read("block_shapes/DC_block_1980/DC_block_1980.shp", quiet=T)
b90_shp <- sf::st_read("block_shapes/nhgis0092_shapefile_tl2000_110_block_1990/DC_block_1990.shp", quiet=T)
b00_shp <- sf::st_read("block_shapes/nhgis0092_shapefile_tl2000_110_block_2000/DC_block_2000.shp", quiet=T)
b10_shp <- sf::st_read("block_shapes/nhgis0092_shapefile_tl2010_110_block_2010/DC_block_2010.shp", quiet=T)
b20_shp <- sf::st_read("block_shapes/nhgis0092_shapefile_tl2020_110_block_2020/DC_block_2020.shp", quiet=T)

b70_df <- readr::read_csv("block_data/nhgis0093_ds96_1970_block.csv")
b80_df <- readr::read_csv("block_data/nhgis_ds104_1980_block_11.csv")
b90_df <- readr::read_csv("block_data/nhgis0092_ds120_1990_block.csv")
b00_df <- readr::read_csv("block_data/nhgis0092_ds147_2000_block.csv")
b10_df <- readr::read_csv("block_data/nhgis0092_ds172_2010_block.csv")
b20_df <- readr::read_csv("block_data/nhgis0092_ds258_2020_block.csv")

invisible(gc())

# rework 1940, 1950 and 1960 data a little bit:
b40_shp <- 
  b40_shp %>% 
  mutate(n_tot = as.numeric(tot_occ), 
         n_white = as.numeric(tot_occ) - as.numeric(nw_occ),
         n_black = n_tot - n_white) %>% 
  select(n_tot, n_black, n_white) %>% 
  mutate(geo_id = as.character(row_number())) %>% mutate(year=1940) %>% 
  sf::st_transform(., 26918) %>%
  mutate(geo_area_meters = sf::st_area(.))
b50_shp <- 
  b50_shp %>% 
  mutate(n_tot = as.numeric(total_occ), 
         n_white = as.numeric(total_occ) - as.numeric(non_white_),
         n_black = n_tot - n_white) %>% 
  select(n_tot, n_black, n_white) %>% 
  mutate(geo_id = as.character(row_number())) %>% mutate(year=1950) %>% 
  sf::st_transform(., 26918) %>%
  mutate(geo_area_meters = sf::st_area(.))

b60_shp <- 
  b60_shp %>% 
  # the 1960 data doesn't have race cross tabs by individual person,
  # but we can take the household race breakdown and apply it to the 
  # population. this is probably roughly accurate. 
  # as a robustness check we can just use the household data and rerun
  # the analysis; see the commented out code.
  mutate(n_tot = as.numeric(stringr::str_remove_all(total_pop, "\\*")), 
         pct_white = (as.numeric(total_occ) - as.numeric(nonwhite_o)) / as.numeric(total_occ)) %>% 
  # mutate(n_tot = as.numeric(total_occ), 
  #        n_white = as.numeric(total_occ) - as.numeric(nonwhite_o)) %>% 
  mutate(n_white = pct_white * n_tot,
         n_black = n_tot - n_white) %>%
  select(n_tot, n_black, n_white) %>% 
  mutate(geo_id = as.character(row_number())) %>% mutate(year=1960) %>% 
  sf::st_transform(., 26918) %>%
  mutate(geo_area_meters = sf::st_area(.))

# 1970 block data has to be specially cleaned, see https://forum.ipums.org/t/race-ethnicity-data-at-a-block-level-from-1970/6178
b70_df$c_black <- b70_df$CM6001 + b70_df$CM6002
b70_df$c_other <- b70_df$CM6003 + b70_df$CM6004
b70_df$c_white <- b70_df$CM5001 + b70_df$CM5002 - b70_df$c_black - b70_df$c_other

# merge the data and the shapefiles and clean up the data:
b70_shp <- clean_block_data(b70_shp, b70_df, "GISJOIN", "GISJOIN", "c_", "c_black", "c_white", year=1970)
b80_shp <- clean_block_data(b80_shp, b80_df, "GISJOIN", "GISJOIN", "C9D0", "C9D002", "C9D001", year=1980)
b90_shp <- clean_block_data(b90_shp, b90_df, "GISJOIN", "GISJOIN", "EUY0", "EUY002", "EUY001", year=1990)
b00_shp <- clean_block_data(b00_shp, b00_df, "GISJOIN", "GISJOIN", "FYE0", "FYE002", "FYE001", year=2000)
b10_shp <- clean_block_data(b10_shp, b10_df, "GISJOIN", "GISJOIN", "H7X", "H7X003", "H7X002", "H7X001", year=2010)
b20_shp <- clean_block_data(b20_shp, b20_df, "GISJOIN", "GISJOIN", "U7J", "U7J003", "U7J002", "U7J001", year=2020)

# remove data we don't need anymore to keep our workspace neat
rm(b70_df, b80_df, b90_df, b00_df, b10_df, b20_df)
invisible(gc())

# fix any broken geometries:
b40_shp <- fix_geo_if_broken(b40_shp)
b50_shp <- fix_geo_if_broken(b50_shp)
b60_shp <- fix_geo_if_broken(b60_shp)
b70_shp <- fix_geo_if_broken(b70_shp)
b80_shp <- fix_geo_if_broken(b80_shp)
b90_shp <- fix_geo_if_broken(b90_shp)
b10_shp <- fix_geo_if_broken(b10_shp)
b20_shp <- fix_geo_if_broken(b20_shp)

# combine shapes into one big object, remove individual objects:
geos_shp <- dplyr::bind_rows(b40_shp, b50_shp, b60_shp, b70_shp, b80_shp, 
                             b90_shp, b00_shp, b10_shp, b20_shp)
# create a truly unique block/tract ID:
geos_shp$geo_id <- paste0(geos_shp$year, "_", geos_shp$geo_id)

# remove objects we don't need anymore
rm(list=ls(pattern="^b[0-9]{2}"))
rm(list=ls(pattern="^t[0-9]{2}"))
# garbage collection
invisible(gc())


####################
# LOAD COVENANT DATA
####################
dc_dcs_shp <- sf::st_read("discriminatory_policies/dc_discriminatory_covenants/dc_discriminatory_covenants.shp", quiet=T)

# remove some shapes that are just artifacts (in PG county)
dc_dcs_shp <-
  filter(dc_dcs_shp, !(fid %in% c(265, 266, 236, 206, 186, 141, 173, 231, 234, 226, 205, 234, 242, 225, 205, 144, 124, 136, 139))) %>%
  sf::st_simplify(., dTolerance = .00002)


leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=dc_dcs_shp %>% st_transform(4326), 
              fillColor = "#F4B942",
              color = "#F4B942",
              stroke=T,
              fillOpacity=1,
              group="Approx. areas w/ discriminatory covenants") %>%
  addLayersControl(
    overlayGroups = c("Approx. areas w/ discriminatory covenants"),
    options = layersControlOptions(collapsed = FALSE)
  ) 



####################
# LOAD ZONE DATA
####################
# load and simplify the zoning map:
# comes from here: https://opendata.dc.gov/datasets/DCGIS::zoning-boundaries-zoning-regulations-of-2016/about
zone_shp <- sf::st_read("zoning/Zoning_Boundaries_(Zoning_Regulations_of_2016).shp", quiet=T)

# list zones in the ZR16 data:
zones_list <- sort(unique(zone_shp$ZR16))
housing_zones <- zones_list[grep(x=zones_list, pattern = "^R|^MU|^NMU")]
# get unzoned areas:
unzoned_shp <- filter(zone_shp, ZR16=='UNZONED')
# subset zones to mostly-housing zones 
zone_shp <- zone_shp[zone_shp$ZR16 %in% housing_zones,]

# create simplified labels
zone_shp$ZR16_simple <- "Other"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^RA-")] <- "Apartment zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^R-")] <- "Residential zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^RF-")] <- "Residential flat zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^MU-")] <- "Mixed use zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^NMU-")] <- "Mixed use zones"

zone_shp <- sf::st_transform(zone_shp, 26918)


# load the 2022 ward shape files
# comes from here: https://opendata.dc.gov/datasets/DCGIS::wards-from-2022/about
ward_shp <- 
  sf::st_read("Wards_from_2022/Wards_from_2022.shp", quiet=T) %>%
  sf::st_transform(26918)

####################
# LOAD FHA DATA
####################

fha_raster <- terra::rast("discriminatory_policies/fha_map/fha_image.tif")

fha_shp <- sf::st_read("discriminatory_policies/fha_map/fha_map.shp", quiet=T) %>% mutate(fha_grade = substr(fha_grade, 1, 1))
fha_centriods <- sf::st_centroid(fha_shp)
fha_centriods$lon <- sf::st_coordinates(fha_centriods)[,1]
fha_centriods$lat <- sf::st_coordinates(fha_centriods)[,2]


race_shp <- 
  geos_shp %>% 
  filter(year==1940) %>% 
  st_transform(4326) %>% 
  mutate(n_nonwhite = n_tot - n_white,
         pct_nonwhite = n_nonwhite / n_tot)




####################
# MAP EVERYTHING
####################
fha_pal <- colorFactor(palette = "RdYlGn", domain = fha_shp$fha_grade, reverse = T)

race_pal <- colorNumeric(palette = "YlOrBr", domain = race_shp$pct_nonwhite)

zone_hatched <- 
  zone_shp %>%
  filter(ZR16 %in% grep(pattern = "R-1", x = sort(unique(zone_shp$ZR16)), value=T)) %>% 
  st_transform(4326) %>%
  hatched.SpatialPolygons(., density = 500)



leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=fha_shp %>% st_transform(4326), 
              fillColor = ~fha_pal(fha_grade),
              stroke=F,
              fillOpacity = .8,
              label=~paste("grade:", fha_grade),
              group="FHA grades from the 30s") %>%
  addPolygons(data=race_shp, 
              fillColor = ~race_pal(pct_nonwhite),
              stroke=F,
              fillOpacity = ~ifelse(is.na(pct_nonwhite), 0, 0.8),
              label=~paste0(round(pct_nonwhite*100, 0), "% non-white residents in 1940"),
              group="% non-white in 1940") %>%
  addLabelOnlyMarkers(lng = fha_centriods$lon, lat = fha_centriods$lat, group = "FHA grade", 
                      label = fha_centriods$fha_grade, labelOptions = c(permanent=T)) %>%
  addPolygons(data=dc_dcs_shp %>% st_transform(4326), 
              fillColor = "#F4B942",
              color = "#F4B942",
              stroke=T,
              fillOpacity=1,
              group="Approx. areas w/ discriminatory covenants") %>%
  addRasterImage(fha_raster, opacity = 0.7, group="FHA map") %>% # Add raster with desired opacity
  addPolygons(data=zone_hatched, 
              fillColor = "#9ffcb1",
              color = "#9ffcb1",
              fillOpacity = .6,
              opacity=.6,
              group="Today's detached house zones 'R-1x'") %>%
  addLayersControl(
    overlayGroups = c("Approx. areas w/ discriminatory covenants", "% non-white in 1940",
                      "FHA grades from the 30s", "FHA grade", "FHA map",
                      "Today's detached house zones 'R-1x'"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(
    position = "bottomright",
    colors = "purple",
    labels = "",
    title = "Current zones that prohibit<br>apartments and duplexes",
    opacity = 1
  ) %>%
  addLegend(
    position = "bottomright",  # Or "topright", "bottomleft", "topleft"
    pal = fha_pal,
    values = fha_shp$fha_grade,
    title = 'FHA "grade"',
    opacity = 1
  ) %>%
  hideGroup("Approx. areas w/ discriminatory covenants") %>%
  hideGroup("FHA map") %>%
  hideGroup("% non-white in 1940") %>%
  hideGroup("FHA grade")




####################
# LOAD PROPERTY DATA
####################
# CAMA data:
# https://opendata.dc.gov/datasets/DCGIS::computer-assisted-mass-appraisal-residential/about
cama <- readr::read_csv("Computer_Assisted_Mass_Appraisal_-_Residential.csv", 
                        show_col_types = F)
# Record lots data:
# https://opendata.dc.gov/datasets/5b0b6b13ef894b8da62e6bd458d907b3_35/explore?location=38.896842%2C-77.002937%2C18.31
rl <- sf::st_read("Record_Lots/Record_Lots.shp", quiet=T)

pd <- 
  dplyr::left_join(rl, cama, by="SSL") %>% 
  filter(!is.na(OBJECTID.y)) %>%
  sf::st_centroid() %>%
  sf::st_transform(26918)

pd$in_r <- 
  lengths(
    sf::st_intersects(pd,
                      filter(zone_shp, 
                             ZR16 %in% grep(pattern = "R-1",
                                            x = sort(unique(zone_shp$ZR16)), 
                                            value=T)
                             )
                      )
    )



wards_2_3 <- ward_shp %>% filter(WARD %in% c(2, 3)) %>% sf::st_union()
pd$in_ward_2_3 <- lengths(st_intersects(pd, wards_2_3))

# number of houses in these zones, average value, and aggregate value
get_stats <- function(df) {
  n <-
    df %>%
    sf::st_drop_geometry() %>%
    summarise(n = n()) %>%
    pull(n)
  
  n <- round(n / 100, 0)*100
  
  avg_p <-
    df %>%
    sf::st_drop_geometry() %>%
    mutate(sale_date = as.numeric(substr(x = SALEDATE, start = 1, stop = 4))) %>%
    filter(sale_date >= 2020) %>%
    summarise(average_value = mean(PRICE, na.rm=T)) %>%
    pull(average_value)
  
  agg_val <- n * avg_p
  
  return(list('n'=n, 'avg_p'=avg_p, "agg_val"=agg_val))
}



leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data=pd[pd$in_r==1 & pd$in_ward_2_3==1,] %>% sf::st_transform(4326),
                   stroke=F)


all <- get_stats(pd %>% filter(in_r==1))
w23 <- get_stats(pd %>% filter(in_r==1 & in_ward_2_3==1))


cat(
 paste0("Number of houses in R-1x zones: ~", format(all[['n']], big.mark=","),
        "\nAverage sale price after 2019: ~", format(all[['avg_p']], big.mark=","),
        "\nEstimated aggregate value: ~", format(all[['agg_val']], big.mark=",")),
        "\n-------------------------------------------",
        "\nNumber of houses in R-1x zones in wards 2 and 3: ~", format(w23[['n']], big.mark=","),
        "\nAverage sale price after 2019 in wards 2 and 3: ~", format(w23[['avg_p']], big.mark=","),
        "\nEstimated aggregate value in wards 2 and 3: ~", format(w23[['agg_val']], big.mark=","),
 
      
        "\nPercent of R-1x homes in wards 2 and 3: ~", round(w23[['n']] / all[['n']] *100,0),
        "\nPercent of aggregate value in wards 2 and 3: ~", round(w23[['agg_val']] / all[['agg_val']] * 100, 0), "%"
    )
