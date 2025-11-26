source("supporting_functions.R")
library(dplyr)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)

# load the historic district boundary shape files
# comes from here: https://opendata.dc.gov/datasets/DCGIS::historic-districts/about 
hd_shp <- sf::st_read("Historic_Districts/Historic_Districts.shp", quiet=T)

hd_data <- readr::read_csv("https://docs.google.com/spreadsheets/d/1Ajl1iAS0NRB7vk_UFDveeWzGkwf3tuiDo-zV9_wtzRM/gviz/tq?tqx=out:csv&sheet=data")

# Merge historic district (HD) data onto HD shapefile, subset to only look at neighborhood HDs:
hd_shp <- dplyr::left_join(x = hd_shp, y = hd_data, by = "UNIQUEID")
hd_shp <- hd_shp[hd_shp$Neighborhood_HD==1,]


# load and simplify the zoning map:
# comes from here: https://opendata.dc.gov/datasets/DCGIS::zoning-boundaries-zoning-regulations-of-2016/about
zone_shp <- sf::st_read("zoning/Zoning_Boundaries_(Zoning_Regulations_of_2016).shp", quiet=T)

# list zones in the ZR16 data:
zones_list <- sort(unique(zone_shp$ZR16))
housing_zones <- zones_list[grep(x=zones_list, pattern = "^R|^MU")]
# subset zones to mostly-housing zones 
zone_shp <- zone_shp[zone_shp$ZR16 %in% housing_zones,]

# create simplified labels
zone_shp$ZR16_simple <- "Other"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^RA-")] <- "Apartment zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^R-")] <- "Residential zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^RF-")] <- "Residential flat zones"
zone_shp$ZR16_simple[grep(x=zone_shp$ZR16, pattern="^MU-")] <- "Mixed use zones"

zone_shp <- select(zone_shp, ZR16_simple)
# fix any broken geometries:
zone_shp <- fix_geo_if_broken(zone_shp)
hd_shp <- fix_geo_if_broken(hd_shp)

# convert to mercator projection
zone_shp <- sf::st_transform(zone_shp, 26918)
hd_shp <- sf::st_transform(hd_shp, 26918)

hd_shp$desig_decade <- hd_shp$desig_date - (hd_shp$desig_date %% 10)
hd_shp$LABEL <- gsub(pattern = " HD", replacement = "", x = hd_shp$LABEL)

hd_shp <- select(hd_shp, LABEL, desig_decade)

# file_names <- list.files("building_permits/")
# i = 1
# for (f in file_names) {
#   if (f ==  "solar_all_years.csv") {next}
#   temp <-
#     readr::read_csv(file.path("building_permits", file_names[i]),
#                     show_col_types = F) %>%
#     filter(
#       grepl("solar|photovolt", DESC_OF_WORK, ignore.case = T) |
#         grepl("solar", PERMIT_SUBTYPE_NAME, ignore.case = T)
#            )
#   year <- as.numeric(stringr::str_sub(string = f, start = -8, end = -5))
#   temp$year <- year
# 
#   if (i==1) {
#     solar <- temp
#   } else {
#     solar <- dplyr::bind_rows(solar, temp)
#   }
#   i <- i + 1
# }
# solar$unique_id <- paste0(solar$DCRAINTERNALNUMBER,
#                           solar$ISSUE_DATE,
#                           solar$PERMIT_ID)
# 
# solar <- solar[!(solar$APPLICATION_STATUS_NAME %in% 
#                   c("EXPIRED", 
#                     "APPLICATION CANCELED", 
#                     "PERMIT CANCELED")),] 
# 
# solar <- 
#   solar %>% 
#   mutate(street = stringr::str_extract(FULL_ADDRESS, "^[^,]+"),
#          zip    = stringr::str_extract(FULL_ADDRESS, "(?<=\\s)\\d+$")) %>%
#   select(year, ISSUE_DATE, PERMIT_SUBTYPE_NAME, DESC_OF_WORK, FULL_ADDRESS, street, zip)
# 
# # write.csv(solar, file = "building_permits/solar_all_years.csv", row.names = F)
# 
# test <- 
#   select(solar, year, ISSUE_DATE, PERMIT_SUBTYPE_NAME, DESC_OF_WORK, FULL_ADDRESS) %>%
#   distinct(.) %>%
#   mutate(FULL_ADDRESS = stringr::str_replace(string = FULL_ADDRESS, 
#                                              pattern = "WASHINGTON, DC", 
#                                              replacement = "WASHINGTON DC,")) %>%
#   mutate(FULL_ADDRESS = stringr::str_remove(FULL_ADDRESS, ",$")) %>%
#   dplyr::left_join(y = readr::read_csv('solar_addresses_geocoded.csv'), by='FULL_ADDRESS')


solar <- readr::read_csv(file = "building_permits/solar_all_years_geocoded.csv", show_col_types = F)

solar_shp <- st_as_sf(solar, coords = c("Geocodio Longitude", "Geocodio Latitude"), crs = 4326) %>% sf::st_transform(26918)



# Data from here: https://opendata.dc.gov/datasets/DCGIS::building-footprints/explore
bf <- 
  sf::st_read("Building_Footprints/Building_Footprints.shp", quiet=T) %>%
  sf::st_transform(26918)




leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group='Carto') %>%
  addPolygons(data=test2 %>% st_transform(4326),
              fillColor = "hotpink",
              fillOpacity = 0.6,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~ZR16,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) 

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group='Carto') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
  addPolygons(group= "HDs",
              data=hd_shp %>% st_transform(4326),
              fillColor = "hotpink",
              fillOpacity = 0.6,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~LABEL,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              ))  %>%
  addCircleMarkers(data=solar,
                   lng = ~`Geocodio Longitude`,
                   lat = ~`Geocodio Latitude`,
                   radius = 2,
                   fillOpacity = .9,
                   stroke = F) %>%
  addLayersControl(baseGroups = c("Carto", "Satellite Imagery"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup("Satellite Imagery")




get_solar_summary <- function(y) {
  # 1. get blocks w/ population
  blocks_w_pop_in_hd <- 
    filter(geos_shp, year==2020 & n_tot > 5) %>%
    # 2. get zone block is in
    sf::st_intersection(y = select(zone_shp, ZR16, ZR16_simple)) %>%
    # 3. get blocks in hds
    sf::st_intersection(y = select(hd_shp, LABEL) %>% st_union(.)) %>%
    filter(!st_is_empty(geometry)) %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    mutate(area_meters = sf::st_area(.)) 
  # 4. count number of solar permits in the block and the number of buildings
  n_permits <- st_intersects(x = blocks_w_pop_in_hd, y = filter(solar_shp, year %in% y))
  blocks_w_pop_in_hd$permit_count <- lengths(n_permits)
  
  n_buildings <- st_intersects(x = blocks_w_pop_in_hd, y = bf)
  blocks_w_pop_in_hd$building_count <- lengths(n_buildings)
  
  blocks_w_pop_in_hd$in_hd <- 1
  
  
  # 1. get blocks w/ population
  blocks_w_pop_no_hd <- 
    filter(geos_shp, year==2020 & n_tot > 2) %>%
    # 2. get zone block is in
    sf::st_intersection(y = select(zone_shp, ZR16, ZR16_simple)) %>%
    # 3. get blocks in hds
    sf::st_difference(y = select(hd_shp, LABEL) %>% st_union(.)) %>%
    filter(!st_is_empty(geometry)) %>%
    sf::st_collection_extract(type = "POLYGON") %>%
    mutate(area_meters = sf::st_area(.))
  # 4. count number of solar permits in the block
  n_permits <- st_intersects(x = blocks_w_pop_no_hd, y = filter(solar_shp, year %in% y))
  blocks_w_pop_no_hd$permit_count <- lengths(n_permits)
  
  n_buildings <- st_intersects(x = blocks_w_pop_no_hd, y = bf)
  blocks_w_pop_no_hd$building_count <- lengths(n_buildings)
  
  blocks_w_pop_no_hd$in_hd <- 0
  
  solar_summary <- 
    dplyr::bind_rows(blocks_w_pop_in_hd, blocks_w_pop_no_hd) %>%
    # 5. sum up number of permits by zone & hd status
    # 6. sum up block zone areas in and out of hds
    # 7. divide number of permits by block areas
    sf::st_drop_geometry(.) %>%
    group_by(in_hd, ZR16_simple) %>%
    summarise(n_permits   = sum(permit_count, na.rm=T),
              n_buildings = sum(building_count, na.rm=T),
              area_meters = sum(as.vector(area_meters), na.rm=T),
              permits_per_acre = n_permits / area_meters * 4046.86,
              permits_as_share_of_buildings = n_permits / n_buildings) %>%
    mutate(year= paste(y, collapse = " "))
    
  
  return(solar_summary)
}


for (y in seq(min(solar$year), max(solar$year))) {
  print(y)
  if (y==min(solar$year)) {
    solar_timeseries <- get_solar_summary(y)
  } else {
    solar_timeseries <- dplyr::bind_rows(get_solar_summary(y), solar_timeseries)
  }
}







ggplot(data = solar_timeseries %>% group_by(year) %>% summarise(n_permits=sum(n_permits)), 
       mapping = aes(x=as.numeric(year), 
                     y = n_permits
       )
) +
  geom_line(size=1.3) + 
  ylab("Number of solar permits") +
  xlab("") +
  theme_minimal() +
  ggtitle('Overall solar adoption is increasing in DC') +
  ggdark::dark_theme_gray() 




solar_summary %>%
  group_by(ZR16_simple) %>%
  summarise(n_permits = sum(n_permits)) %>%
  ggplot(data = ., 
         mapping = aes(x=as.factor(ZR16_simple), 
                       y = n_permits
         )
  ) +
  geom_bar(stat="identity", position="dodge") + 
  ggtitle("More than half of all solar permits are in residential zones") +
  xlab("") +
  theme_minimal() +
  ylab('Total solar permits') +
  labs(caption = "Graph shows solar permits accross all years in both HDs and outside") +
  ggdark::dark_theme_gray()



to_plot <-
  solar_timeseries %>% 
  group_by(year, in_hd) %>% 
  summarise(n_permits=sum(n_permits),
            n_buildings=sum(n_buildings),
            permits_as_share_of_buildings = n_permits / n_buildings)

ggplot(data = to_plot, 
       mapping = aes(x=as.numeric(year), 
                     y = permits_as_share_of_buildings,
                     linetype = forcats::fct_rev(as.factor(in_hd))
       )
) +
  geom_line(size=1.3) + 
  ylab("Solar permits as % of total buildings") +
  xlab("") +
  labs(linetype="") +
  theme_minimal() +
  ggtitle('Solar installations have taken off in DC, but less so in HDs') +
  scale_linetype_discrete(
    labels = c("Outside HDs", "In HDs"),
    breaks=levels(as.factor(to_plot$in_hd))
    ) + 
  scale_y_continuous(labels = scales::percent) +
  ggdark::dark_theme_gray() 





to_plot <- solar_timeseries %>% 
  group_by(ZR16_simple, in_hd) %>% 
  mutate(permits_as_share_of_buildings = sum(n_permits / n_buildings, na.rm=T))

ggplot(to_plot,
       aes(x=ZR16_simple, y=permits_as_share_of_buildings, fill=rev(as.factor(in_hd)))) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  labs(fill="") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Solar permits as %  of total buildings") +
  xlab("") +
  ggtitle("Solar adoption lags in HDs in apartment, mixed-use, and residential zones") +
  scale_fill_discrete(
    labels = c("In HDs", "Outside HDs"),
    breaks=levels(as.factor(to_plot$in_hd))
  ) + 
  ggdark::dark_theme_gray()
  















b <- geos_shp[geos_shp$year==2020,]
b$n_buildings <- lengths(st_intersects(x = b, y = bf))
b$n_permits <- lengths(st_intersects(x = b, y = solar_shp))

b$pct_buildings <- b$n_permits / b$n_buildings
b$pct_buildings[is.nan(b$pct_buildings)] <- NA
b <- b[b$n_tot > 5,]
b$pct_buildings[b$pct_buildings > 1] <- 1


b$pct_buildings_quantiles <- 
  cut(x = b$pct_buildings*100, 
      breaks = c(0, .03, 0.085, .14, .33, 1)*100, 
      include.lowest = T
      )

pct_pal <- colorFactor(palette = "RdYlGn", 
                        reverse = F, 
                        domain = b$pct_buildings_quantiles, na.color = "#FFFFFF00")
factpal <- colorFactor(palette = "Set1", domain = zone_shp$ZR16_simple)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group='Carto') %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Imagery") %>%
  addPolygons(group='blocks',
    data=b %>% st_transform(4326),
              fillColor = ~pct_pal(pct_buildings_quantiles),
              fillOpacity = 0.4,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~paste0(round(pct_buildings*100, 0), "%"),
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addPolygons(group='hds',
              data=hd_shp %>% st_transform(4326),
              fillColor = 'grey',
              fillOpacity = .1,
              weight = 3,
              opacity = 1,
              color = "blue",
              label=~paste(LABEL),
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addHeatmap(group = 'permit heatmap',
             data = solar,
             lng = ~`Geocodio Longitude`,
             lat = ~`Geocodio Latitude`,
             radius = 20,           # Adjust radius of heatmap points
             blur = 15,             # Adjust blur amount
             gradient = c("blue", "cyan", "limegreen", "yellow", "red")
             ) %>% # Customize color gradient)
  addLegend(group='blocks',
            pal = pct_pal, values = b$pct_buildings_quantiles, 
            opacity = 0.7, 
            # labFormat = labelFormat(big.mark = ""),
            title = "block-aggregated data:<br>solar permits as a % of<br>total building count") %>%
  addCircleMarkers(group="permits",
                   data=solar,
                   lng = ~`Geocodio Longitude`,
                   lat = ~`Geocodio Latitude`,
                   radius = 2,
                   fillOpacity = .9,
                   stroke = F) %>%
  addPolygons(group="ZR16 zones",
              data=zone_shp %>% st_transform(4326),
              fillColor = ~factpal(ZR16_simple), # Apply the color function
              fillOpacity = 0.7,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~ZR16,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = F
              )
  ) %>%
  addLayersControl(overlayGroups = c("blocks", "hds", "permits", 
                                     "permit heatmap", "ZR16 zones"), 
                   baseGroups = c("Carto", "Satellite Imagery"),
                   options = layersControlOptions(collapsed = F)
                   ) %>%
  hideGroup("permits") %>% hideGroup("blocks") %>% hideGroup("ZR16 zones")






# WEATHERNORMALZEDSITEEUI_KBTUFT (Weather Normalized Site Energy Use Intensity)
# Building energy benchmarking data from Open Data DC:
beb <- 
  readr::read_csv("building_energy_data/Building_Energy_Benchmarking.csv",
                  show_col_types = F) %>%
  filter(!is.na(WEATHERNORMALZEDSITEEUI_KBTUFT)) %>%
  filter(WEATHERNORMALZEDSITEEUI_KBTUFT > 0) 

beb_shp <- st_as_sf(
  beb, 
  coords = c("LONGITUDE", "LATITUDE"), 
  crs = 4326
)


beb_shp$in_hd <- 
  lengths(st_intersects(x = beb_shp %>% sf::st_transform(26918), y = hd_shp))
beb_shp$in_hd[beb_shp$in_hd > 1] <- 1

beb_shp_zone <- 
  sf::st_join(x = beb_shp %>% sf::st_transform(26918), 
              y = zone_shp, how='left')


temp <- beb_shp_zone %>%
  sf::st_drop_geometry(.) %>%
  filter(!is.na(PRIMARYPROPERTYTYPE_EPACALC)) %>%
  group_by(in_hd, ZR16_simple, PRIMARYPROPERTYTYPE_EPACALC) %>%
  summarize(median_energy_use = median(TOTGHGEMISSINTENSITY_KGCO2EFT, na.rm=T),
            count = n()) %>%
  tidyr::pivot_wider(names_from = in_hd, 
                     values_from = c(median_energy_use,
                                     count)) %>%
  mutate(total_count = count_0 + count_1,
         diff = median_energy_use_0 - median_energy_use_1) %>%
  arrange(-total_count)

beb_shp_zone %>%
  sf::st_drop_geometry(.) %>%
  filter(!is.na(PRIMARYPROPERTYTYPE_EPACALC)) %>%
  mutate(
         `In HD (1=yes)`    = in_hd,
         `building vintage` = case_when(
           YEARBUILT < 1950                      ~ "Built before 1950",
           YEARBUILT <= 1990 & YEARBUILT >= 1950 ~ "Built btw 1950 & 1990",
           YEARBUILT > 1990                      ~ "Built post-1990"
           )
         ) %>%
  group_by(`In HD (1=yes)`, 
           `building vintage`, 
           PRIMARYPROPERTYTYPE_EPACALC
           ) %>%
  summarize(median_energy_use = median(WEATHERNORMALZEDSITEEUI_KBTUFT, na.rm=T),
            `number of buildings` = n()) %>%
  ungroup() %>%
  filter(`number of buildings` >= 100) %>%
  group_by(`building vintage`, `PRIMARYPROPERTYTYPE_EPACALC`) %>%
  mutate(c = n()) %>%
  filter(c > 1) %>%
  ungroup() %>%
  rename(
    `Median kg CO2e per sqft` = median_energy_use
    ) %>%
  rename(`building type` = PRIMARYPROPERTYTYPE_EPACALC) %>%
  select(-c) %>%
  arrange(`building vintage`, `building type`, `In HD (1=yes)`) %>%
  View(.)



test2 <-
  test %>%
  select(TOTGHGEMISSINTENSITY_KGCO2EFT, in_hd, PRIMARYPROPERTYTYPE_EPACALC, ZR16_simple, ZR16) %>%
  filter(PRIMARYPROPERTYTYPE_EPACALC %in% c("Mixed Use Property", "Hotel",  "K-12 School", "Office", "Multifamily Housing")) %>%
  filter(TOTGHGEMISSINTENSITY_KGCO2EFT < 100) %>%
  filter(TOTGHGEMISSINTENSITY_KGCO2EFT > 0) 

mymodel <- 
  lm(formula = TOTGHGEMISSINTENSITY_KGCO2EFT ~ as.factor(in_hd) + as.factor(PRIMARYPROPERTYTYPE_EPACALC) + as.factor(ZR16_simple), 
     data= test2 
     )
  

ggplot() +
  geom_density(aes(x = test2$TOTGHGEMISSINTENSITY_KGCO2EFT), fill="blue")

summary(mymodel)
plot(mymodel)

summary(test2$TOTGHGEMISSINTENSITY_KGCO2EFT)

0.2 / 4.5
  
sum(temp$total_count*temp$median_energy_use_1, na.rm = T) / sum(temp$total_count, na.rm = T)
sum(temp$total_count*temp$median_energy_use_0, na.rm = T) / sum(temp$total_count, na.rm = T)

beb_shp %>%
  sf::st_drop_geometry(.) %>%
  filter(!is.na(PRIMARYPROPERTYTYPE_EPACALC)) %>%
  group_by(in_hd) %>%
  summarize(median_energy_use = median(WEATHERNORMALZEDSITEEUI_KBTUFT, na.rm=T),
            count = n()) %>%
  tidyr::pivot_wider(names_from = in_hd, 
                     values_from = c(median_energy_use,
                                     count)) %>%
  mutate(total_count = count_0 + count_1,
         diff = median_energy_use_0 - median_energy_use_1) %>%
  arrange(-total_count) %>%
  View()



# blg <- sf::st_read("building_energy_data/Building_Energy_Performance/Building_Energy_Performance.shp", quiet=T) %>%
#   sf::st_transform(26918)
# 
# 
# beps_pal <- colorFactor(palette = "Set1", 
#                        reverse = F, 
#                        domain = as.factor(blg$MEETS_BEPS), na.color = "#FFFFFF00")
# 
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group='Carto') %>%
  addCircleMarkers(group="buildings",
                   data=beb,
                   lng = ~LONGITUDE,
                   lat = ~LATITUDE,
                   color = "blue",
                   radius = 4,
                   fillOpacity = .9,
                   stroke = F) %>%
  addPolygons(group='hds',
              data=hd_shp %>% st_transform(4326),
              fillColor = 'grey',
              fillOpacity = .1,
              weight = 3,
              opacity = 1,
              color = "blue",
              label=~paste(LABEL),
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addLayersControl(overlayGroups = c("buildings", "hds"),
                   options = layersControlOptions(collapsed = F)
  ) 
# 
# blg$in_hd <- lengths(st_intersects(x = blg, y = hd_shp))
# blg$in_hd[blg$in_hd > 1] <- 1
# 
# 
# blg %>% 
#   sf::st_drop_geometry(.) %>%
#   filter(MEETS_BEPS %in% c("Does Not Meet BEPS", "Meets BEPS", "Does Not Meet BEPS: Incomplete/Missing Report")) %>%
#   filter(!is.na(PRIMARYPRO)) %>%
#   mutate(meets_beps = ifelse(MEETS_BEPS == "Meets BEPS", 1, 0)) %>%
#   group_by(in_hd, PRIMARYPRO) %>%
#   summarize(meets_beps_pct = mean(meets_beps),
#             count = n()) %>%
#   tidyr::pivot_wider(names_from = in_hd, values_from = c(meets_beps_pct, count)) %>%
#   mutate(diff = meets_beps_pct_0 - meets_beps_pct_1) %>%
#   View()




# # Create a frequency table
# blg_subset <- blg %>% filter(MEETS_BEPS %in% c("Does Not Meet BEPS", "Meets BEPS"))
# freq_table <- table(blg$in_hd, blg$MEETS_BEPS)
# 
# # Calculate proportions (percentages)
# percent_table <- prop.table(freq_table, margin=1) * 100  
# percent_table
# 
# 
# blg %>%
#   sf::st_drop_geometry(.) %>%
#   mutate(DISTANCE_F = as.numeric(stringr::str_remove(string = DISTANCE_F, pattern = "%"))) %>%
#   mutate(old = ifelse(YEARBUILT < 2010, 1, 0)) %>%
#   group_by(in_hd) %>%
#   summarize(q5 = quantile(DISTANCE_F, 0.05, na.rm=T),
#             q25 = quantile(DISTANCE_F, 0.25, na.rm=T),
#             median = quantile(DISTANCE_F, 0.50, na.rm=T),
#             q75 = quantile(DISTANCE_F, 0.75, na.rm=T),
#             q95 = quantile(DISTANCE_F, 0.95, na.rm=T),
#             count = n()
#             ) %>%
#   filter(!is.na(median)) %>% filter(count > 5)
# 
# blg %>%
#   sf::st_drop_geometry(.) %>%
#   mutate(DISTANCE_F = as.numeric(stringr::str_remove(string = DISTANCE_F, pattern = "%"))) %>%
#   filter(MEETS_BEPS == 'Does Not Meet BEPS') %>%
#   ggplot() +
#   geom_density(
#     aes(DISTANCE_F, fill=as.factor(in_hd)),
#     alpha=.5
#     ) +
#   theme_minimal() +
#   ggdark::dark_theme_gray()






es <- 
  readr::read_csv("building_energy_data/energy_star_buildings_geocoded.csv", 
                  show_col_types = F) %>%
  mutate(latest_score = 
           as.numeric(substr(`Score(s)`, nchar(`Score(s)`) - 1, nchar(`Score(s)`)))) %>%
  filter(latest_score > 0) 

es_shp <- st_as_sf(es, coords = c("Geocodio Longitude", "Geocodio Latitude"), crs = 4326) %>%
  sf::st_transform(st_crs(hd_shp))

pal <- colorNumeric(palette = "RdYlGn", domain = es$latest_score)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group='Carto') %>%
  addCircleMarkers(group="buildings",
                   data=es,
                   lng = ~`Geocodio Longitude`,
                   lat = ~`Geocodio Latitude`,
                   color = ~pal(latest_score),
                   radius = 4,
                   label = ~latest_score,
                   fillOpacity = .9,
                   stroke = F) %>%
  addPolygons(group='hds',
              data=hd_shp %>% st_transform(4326),
              fillColor = 'grey',
              fillOpacity = .1,
              weight = 3,
              opacity = 1,
              color = "blue",
              label=~paste(LABEL),
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addLayersControl(overlayGroups = c("buildings", "hds"), 
                   options = layersControlOptions(collapsed = F)
  ) 

es_shp$in_hd <- lengths(sf::st_intersects(x = es_shp, y = hd_shp))
es_shp$in_hd[es_shp$in_hd > 1] <- 1

ggplot(es_shp %>% sf::st_drop_geometry(.)) +
  geom_density(
    aes(latest_score, fill=as.factor(in_hd)),
    alpha=.5
  ) +
  theme_minimal() +
  ggdark::dark_theme_gray()


bp <- readr::read_csv("building_permits/Building_Permits_in_2024.csv", show_col_types = F)
bp

bp_shp <- st_as_sf(bp, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  sf::st_transform(st_crs(hd_shp))

bp_shp$in_hd <- lengths(sf::st_intersects(x = bp_shp, y = hd_shp))
bp_shp$in_hd[bp_shp$in_hd > 1] <- 1


bp_shp %>%
  sf::st_drop_geometry(.) %>%
  group_by(in_hd) %>%
  summarize(q5 = quantile(FEES_PAID, 0.05, na.rm=T),
            q25 = quantile(FEES_PAID, 0.25, na.rm=T),
            median = quantile(FEES_PAID, 0.50, na.rm=T),
            mean = mean(FEES_PAID, na.rm=T),
            q75 = quantile(FEES_PAID, 0.75, na.rm=T),
            q95 = quantile(FEES_PAID, 0.95, na.rm=T),
            count = n()
  ) %>%
  filter(!is.na(median)) %>% filter(count > 5) %>%
  knitr::kable()



create_freqs <- function(v) {
  my_corpus <- Corpus(VectorSource(v))
  
  my_corpus <- tm_map(my_corpus, content_transformer(tolower)) # Convert to lowercase
  my_corpus <- tm_map(my_corpus, removeNumbers) # Remove numbers
  my_corpus <- tm_map(my_corpus, removePunctuation) # Remove punctuation
  my_corpus <- tm_map(my_corpus, removeWords, stopwords("english")) # Remove common English stop words
  my_corpus <- tm_map(my_corpus, stripWhitespace) # Remove extra whitespace
  my_corpus <- tm_map(my_corpus, stemDocument)
  
  tdm <- TermDocumentMatrix(my_corpus)
  matrix_tdm <- as.matrix(tdm)
  word_frequencies <- sort(rowSums(matrix_tdm), decreasing = TRUE)
  df_word_frequencies <- 
    data.frame(word = names(word_frequencies), freq = word_frequencies) %>%
    mutate(pct = freq / length(v))
  
  return(df_word_frequencies)
}

hd_freqs <-
  bp_shp %>% 
  sf::st_drop_geometry(.) %>% 
  filter(in_hd==1) %>%
  filter(!is.na(DESC_OF_WORK)) %>% 
  pull(DESC_OF_WORK) %>% 
  create_freqs(.)

no_hd_freqs <-
  bp_shp %>% 
  sf::st_drop_geometry(.) %>% 
  filter(in_hd==0) %>%
  filter(!is.na(DESC_OF_WORK)) %>% 
  pull(DESC_OF_WORK) %>% 
  create_freqs(.)

all_freqs <- 
  dplyr::inner_join(x = hd_freqs, y = no_hd_freqs, by="word") %>%
  mutate(diff = pct.x - pct.y) #%>%
  tidyr::pivot_longer(cols = c(pct.x, pct.y),
                      names_to = "in_hd",
                      values_to = "percent")


plot1 <-
  ggplot(all_freqs %>% 
         arrange(-diff) %>% 
         slice(1:10) %>%
         mutate(word = forcats::fct_reorder(word, diff))
       ) +
  geom_segment( aes(x=word, xend=word, y=pct.x, yend=pct.y), color="grey") +
  geom_point( aes(x=word, y=pct.x, color="#F8766D"), size=3 ) +
  geom_point( aes(x=word, y=pct.y, color="#00B0F6"), size=3 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("% of permits") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = "",
    values = c("#F8766D" = "#F8766D", "#00B0F6" = "#00B0F6"),
    labels = c("Outside HD", "In HD")
  ) +
  ggtitle("Words that are more common in permits in HDs") +
  theme_minimal() +
  ggdark::dark_theme_gray() +
  guides(color = guide_legend(override.aes = list(size = 5))) + # Increase point size in legend
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

plot2 <-
  ggplot(all_freqs %>% 
           arrange(diff) %>%
           slice(1:10) %>%
           mutate(word = forcats::fct_reorder(word, -diff))
  ) +
  geom_segment( aes(x=word, xend=word, y=pct.x, yend=pct.y), color="grey") +
  geom_point( aes(x=word, y=pct.x, color="#F8766D"), size=3 ) +
  geom_point( aes(x=word, y=pct.y, color="#00B0F6"), size=3 ) +
  coord_flip()+
  theme_minimal() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("% of permits") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = "",
    values = c("#F8766D" = "#F8766D", "#00B0F6" = "#00B0F6"),
    labels = c("Outside HD", "In HD")
  ) +
  ggtitle("Words that are more common in permits outside of HDs") +
  theme_minimal() +
  ggdark::dark_theme_gray() +
  guides(color = guide_legend(override.aes = list(size = 5))) + # Increase point size in legend
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))


grid.arrange(plot1, plot2, ncol = 2)




















###############################################################################
# Building efficiency data
###############################################################################













to_run <- ts1 %>% 
  # filter out rows that are missing the outcome variable
  filter(!is.na(!!sym("pop_density"))) %>% 
  mutate(row_num = row_number()) %>%
  group_by(pair_id, year) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  # create year variable that's relative to the t0 year
  mutate(rel_year = (year - desig_decade) / 10,
         treated = treat,
         did_post = treat*desig_decade) %>%
  filter(hd_name != "Colony Hill") %>%
  group_by(hd_name, pair_id) %>% 
  mutate(count=n()) %>% 
  filter(count==18) %>% 
  ungroup()



attgt <- did::att_gt(yname = "pop_density",
                     gname = "did_post",
                     idname = "did_unique_id",
                     tname = "year",
                     xformla = ~1,
                     data =  to_run,
                     clustervars = "pair_id",
                     weightsname="n_tot",
                     allow_unbalanced_panel = T,
                     base_period = "universal",
                     control_group = "nevertreated",
                     
)

test <- show_buffer_att_results(dep_var="pop_density", title = "Population Density: buffer approach (all HDs)", 
                        buffer_size=800, beta_reg=F, show_resid_plots = F)

library(HonestDiD)


summary(test)
es <- did::aggte(test, type = "dynamic")
ggdid(es)

#Run sensitivity analysis for relative magnitudes
sensitivity_results <-
  honest_did(es,
             e=0,
             type="relative_magnitude",
             Mbarvec=seq(from = 0.5, to = 2, by = 0.5))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci)








#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#'
#' @param ... Parameters to pass to the relevant method.
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param es Result from aggte (object of class AGGTEobj).
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @param gridPoints Number of grid points used for the underlying test
#'  inversion. Default equals 100. User may wish to change the number of grid
#'  points for computational reasons.
#' @param ... Parameters to pass to `createSensitivityResults` or
#'  `createSensitivityResults_relativeMagnitudes`.
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # Recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Check time vector is consecutive with referencePeriod = -1
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if ( consecutivePre | consecutivePost ) {
    msg <- "honest_did expects a time vector with consecutive time periods;"
    msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
    stop(msg)
  }
  
  # Remove the coefficient normalized to zero
  hasReference <- any(es$egt == referencePeriod)
  if ( hasReference ) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex,-referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < referencePeriod))
  npost    <- nperiods - npre
  if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
    if ( npost <= 0 ) {
      msg <- "not enough post-periods"
    } else {
      msg <- "not enough pre-periods"
    }
    msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
    stop(msg)
  }
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}




























buffer_size = 100
dep_var = "pop_density"
cvars = "LABEL"
hds_to_omit <-
  c("Emerald St", 
    "Emerald St HD",
    "Grant Rd", 
    "Grant Circle",
    "Union Market",
    "Lafayette Square",
    "Grant Rd HD", 
    "Mount Vernon Triangle HD",
    "Mount Vernon Triangle",
    "Pennsylvania Ave NHS HD",
    "Pennsylvania Ave NHS",
    "Grant Circle HD", "Union Market HD",
    # "Downtown",
    # "Downtown HD",
    "Lafayette Square HD")

pa <- sf::st_read("Comprehensive_Plan_Planning_Areas/Comprehensive_Plan_Planning_Areas.shp", quiet=T)

# Join poly_A to poly_B based on the largest overlapping area
# The result will have a new column indicating the overlap details
# hd_shp <- 
#   sf::st_join(hd_shp, 
#               pa %>% 
#                 sf::st_transform(sf::st_crs(hd_shp)) %>%
#                 select(NAME) %>%
#                 rename(planning_area = NAME), 
#               join = st_intersects, 
#               largest = T,
#               left = T)

hd_subset <- filter(hd_shp, !(LABEL %in% hds_to_omit))
ob <- sf::st_buffer(x = filter(hd_shp, !(LABEL %in% hds_to_omit)), dist = buffer_size)
ib <- sf::st_buffer(x = filter(hd_shp, !(LABEL %in% hds_to_omit)), dist = -1*(buffer_size))

outer_buffer <- sf::st_difference(x = ob, y = sf::st_union(hd_subset))
inner_buffer <- sf::st_difference(x = hd_subset, y = sf::st_union(ib))

nohd_blocks <- sf::st_intersection(x = geos_shp[geos_shp$geo_id %in% geos_outside_hds,], outer_buffer) %>% mutate(treat=0)
hd_blocks <- sf::st_intersection(x = geos_shp, inner_buffer) %>% mutate(treat=1) 

# table(hd_blocks$geo_id %in% nohd_blocks$geo_id)
# table(nohd_blocks$geo_id %in% hd_blocks$geo_id)

nohd_blocks <- 
  nohd_blocks %>%
  filter(!(geo_id %in% geos_in_hds)) %>%
  filter(n_tot > 10)

hd_blocks <-
  hd_blocks %>%
  filter(!(geo_id %in% geos_outside_hds)) %>%
  filter(n_tot > 10)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(group= "nohd_blocks",
              data=nohd_blocks %>% sf::st_transform(4326) %>%
                filter(n_tot > 5),
              fillColor = "red",
              fillOpacity = 0.5,
              weight = 1,
              opacity = 1,
              color = "white",
              # label=~LABEL,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addPolygons(group= "inner_buffer",
              data=geos_shp[geos_shp$geo_id %in% hd_blocks$geo_id & geos_shp$year == 2020,] %>% sf::st_transform(4326) %>%
                filter(n_tot > 5),
              fillColor = "blue",
              fillOpacity = 0.5,
              weight = 1,
              opacity = 1,
              color = "white",
              # label=~LABEL,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addPolygons(group= "outer_buffer",
              data=geos_shp[geos_shp$geo_id %in% nohd_blocks$geo_id & geos_shp$year == 2020,] %>% sf::st_transform(4326) %>%
                filter(n_tot > 5),
              fillColor = "green",
              fillOpacity = 0.5,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~paste(geo_id, n_tot),
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addPolygons(group= "HDs",
              data=hd_shp %>% sf::st_transform(4326),
              fillColor = "hotpink",
              fillOpacity = 0.5,
              weight = 1,
              opacity = 1,
              color = "white",
              label=~LABEL,
              highlightOptions = highlightOptions(weight = 3,
                                                  color = "white",
                                                  bringToFront = FALSE
              )) %>%
  addLayersControl(overlayGroups = c("HDs", "inner_buffer", "outer_buffer", "nohd_blocks"))

# get pct overlap
# i_hds_geos <-
#   sf::st_intersection(x = geos_shp, y = sf::st_union(hd_shp %>% filter(!(LABEL %in% hds_to_omit)))) %>%
#   mutate(i_area = sf::st_area(.)) %>%
#   mutate(pct_overlap = as.vector(i_area / geo_area_meters)) %>%
#   sf::st_drop_geometry(.)


# hd_blocks %>%
#   group_by(LABEL, year) %>%
#   mutate(block_area_acres = sum(as.vector(geo_area_meters) / 4046.86, na.rm=T)) %>%
#   arrange(year, block_area_acres) %>%
#   View()



buffer_ts <- 
  dplyr::bind_rows(hd_blocks, nohd_blocks) %>%
  sf::st_drop_geometry(.) %>%
  mutate(pop_density = n_tot / as.vector(geo_area_meters),
         pct_black = n_black / n_tot,
         pct_white = n_white / n_tot) %>%
  mutate(post = ifelse(year > desig_decade, 1, 0),
         did_post = ifelse(treat==1, desig_decade+10, 0)) %>%
  group_by(year, LABEL, treat, geo_id) %>%
  mutate(did_unique_id = cur_group_id()) %>%
  ungroup() %>%
  select(geo_id, did_unique_id, LABEL, year, treat, post, did_post, 
         desig_decade, pop_density, n_black, n_white, n_tot, 
         pct_black, pct_white, geo_area_meters) %>%
  # filter(!(LABEL %in% hds_filter_out)) %>%
  mutate(pop_density = pop_density * 4046.86) %>%
  filter(n_tot > 10) %>%
  mutate(rel_year = (year - desig_decade) / 10) %>%
  # dplyr::left_join(y = i_hds_geos %>% select(pct_overlap, geo_id), by = "geo_id") %>%
  # dplyr::left_join(y = select(hd_shp, planning_area, LABEL), by="LABEL") 
  dplyr::left_join(y = hd_shp %>% 
                     mutate(hd_area_acres = as.vector(sf::st_area(.)) / 4046.86 ) %>%
                     sf::st_drop_geometry(.) %>% 
                     select(LABEL, hd_area_acres),
                   by = "LABEL")


# buffer_ts %>% group_by(LABEL, treat, post) %>%
#   summarize(mean_pop_dens = mean(pop_density, na.rm=T)) %>%
#   mutate(change = round(mean_pop_dens - lag(mean_pop_dens), 1)) %>%
#   View(.)
# 
attgt <- did::att_gt(yname = dep_var,
                     gname = "did_post",
                     idname = "did_unique_id",
                     tname = "year",
                     xformla = ~1,
                     data =  buffer_ts, #%>% filter(LABEL %in% c("Georgetown", "Capitol Hill")),
                     clustervars = "did_unique_id",#cvars,
                     weightsname="n_tot",
                     allow_unbalanced_panel = F,
                     base_period = "varying",
                     panel = F
)
summary(attgt)
group_effects <- aggte(attgt, type = "simple", na.rm = T)
summary(group_effects)
ggdid(group_effects) +
        ggdark::dark_theme_gray()




to_plot <-
  buffer_ts %>%
  # group_by(LABEL) %>%
  # # mutate(hd_2020_pop = ifelse(year==2020 & treat==1, sum(n_tot, na.rm=T), 0)) %>%
  # ungroup() %>%
  group_by(LABEL, treat, post) %>%
  summarize(mean_pop_dens = mean(pop_density, na.rm=T),
            hd_area_acres = max(hd_area_acres, na.rm=T)) %>%
  mutate(change = round(mean_pop_dens - lag(mean_pop_dens), 1)) %>%
  filter(!is.na(change)) %>%
  ungroup()

  

to_plot_2 <-
  to_plot %>%
  group_by(LABEL) %>%
  arrange(LABEL, -treat) %>%
  mutate(diff_in_diff = change - lag(change)) %>%
  filter(!is.na(diff_in_diff))

ggplot(data=to_plot) +
  geom_point(
    aes(x=forcats::fct_reorder(factor(LABEL), hd_area_acres), 
        y=change, 
        color=forcats::fct_rev(factor(treat)),
        size=hd_area_acres
        )
    ) +
  coord_flip() +
  ggdark::dark_theme_gray()

ggplot(data=to_plot_2) +
  geom_point(
    aes(x=forcats::fct_reorder(factor(LABEL), hd_area_acres), 
        y=diff_in_diff, 
        size=hd_area_acres
    )
  ) +
  geom_hline(aes(yintercept = 0), col='red') +
  xlab("") +
  ylab("") +
  coord_flip() +
  ggdark::dark_theme_gray()

sum((to_plot_2$diff_in_diff * to_plot_2$hd_area_acres) / sum(to_plot_2$hd_area_acres))




do_buffer_analysis <- function(buffer_size, hds_to_omit, cvars, 
                               dep_var, 
                               return_shps=F,
                               unzoned_threshold=1.1) {
  
  # remove tiny HDs or HDs that are mostly office / government buildings
  hd_subset <- filter(hd_shp, !(LABEL %in% hds_to_omit))
  
  # get the "inner" and "outer" buffers
  ob <- sf::st_buffer(x = filter(hd_shp, !(LABEL %in% hds_to_omit)), dist = buffer_size)
  ib <- sf::st_buffer(x = filter(hd_shp, !(LABEL %in% hds_to_omit)), dist = -1*(buffer_size))
  
  # create the inner and outer bands on either side of the HD boundary, which will
  # extend buffer_size meters on either side of the boundary
  outer_buffer <- sf::st_difference(x = ob, y = sf::st_union(hd_subset))
  inner_buffer <- sf::st_difference(x = hd_subset, y = sf::st_union(ib))
  
  # any block that touches that band will be included in the analysis
  # the blocks that touch the inner band will be the "treatment" group
  # the blocks that touch the outer band will be the "control" group
  # using treatment and control here very loosely tho as this is not a 
  # randomized or pseudo-randomized analysis and we're really doing
  # descriptive statistics
  # so think of "treatment" as "in HD" and "control" as "outside HD"
  nohd_blocks <- sf::st_intersection(x = geos_shp[geos_shp$geo_id %in% geos_outside_hds,], outer_buffer) %>% mutate(treat=0)
  hd_blocks <- sf::st_intersection(x = geos_shp, inner_buffer) %>% mutate(treat=1) 
  
  # make sure we don't have any overlap between the HD and non-HD blocks,
  # and remove blocks with less than 10 people
  nohd_blocks <- 
    nohd_blocks %>%
    filter(!(geo_id %in% geos_in_hds)) %>%
    filter(n_tot > 10)
  
  hd_blocks <-
    hd_blocks %>%
    filter(!(geo_id %in% geos_outside_hds)) %>%
    filter(n_tot > 10)
  
  # create the timeseries by stacking the in-HD blocks and the outside-HD blocks
  buffer_ts <- 
    dplyr::bind_rows(hd_blocks, nohd_blocks) %>%
    sf::st_drop_geometry(.) %>%
    # create our main variables of interest
    mutate(pop_density = n_tot / as.vector(geo_area_meters),
           pct_black = n_black / n_tot,
           pct_white = n_white / n_tot) %>%
    # create some indicator and ID variables we'll need
    mutate(post = ifelse(year > desig_decade, 1, 0),
           did_post = ifelse(treat==1, desig_decade+10, 0)) %>%
    group_by(year, LABEL, treat, geo_id) %>%
    mutate(did_unique_id = cur_group_id()) %>%
    ungroup() %>%
    select(geo_id, did_unique_id, LABEL, year, treat, post, did_post, 
           desig_decade, pop_density, n_black, n_white, n_tot, 
           pct_black, pct_white, geo_area_meters) %>%
    # get the population density
    mutate(pop_density = pop_density * 4046.86) %>%
    # again, remove smaller blocks (I am paranoid lol)
    filter(n_tot > 10) %>%
    # get a "relative year" variable, which is 0 in the decade the HD was designated
    mutate(rel_year = (year - desig_decade) / 10) %>%
    # merge on the # of acres in the HD
    dplyr::left_join(y = hd_shp %>% 
                       mutate(hd_area_acres = as.vector(sf::st_area(.)) / 4046.86 ) %>%
                       sf::st_drop_geometry(.) %>% 
                       select(LABEL, hd_area_acres),
                     by = "LABEL")
  
  # remove blocks that are over 80% unzoned land
  overlap_i <-
    sf::st_intersection(x = geos_shp %>%
                          filter(geo_id %in% unique(c(hd_blocks$geo_id, nohd_blocks$geo_id))) %>%
                          select(geo_id) %>%
                          distinct(.),
                        y = sf::st_union(unzoned_shp)) %>%
    mutate(overlap_area = as.vector(sf::st_area(.))) %>%
    sf::st_drop_geometry(.) %>%
    dplyr::right_join(y = buffer_ts, by='geo_id') %>%
    mutate(pct_overlap = overlap_area / as.vector(geo_area_meters),
           threshold   = ifelse(pct_overlap >= unzoned_threshold, 1, 0))

  blocks_w_mostly_unzoned_land <-
    unique(overlap_i$geo_id[overlap_i$threshold==1 &
                              !is.na(overlap_i$threshold)])
  buffer_ts <-
    filter(buffer_ts, !(geo_id %in% blocks_w_mostly_unzoned_land))
  
  # create data sets to plot changes in density & demographics before / after HD creation
  to_plot <-
    buffer_ts %>%
    group_by(LABEL, treat, post) %>%
    summarize(outcome_var = mean(.data[[dep_var]], na.rm=T),
              hd_area_acres = max(hd_area_acres, na.rm=T)) %>%
    mutate(change = round(outcome_var - lag(outcome_var), 1)) %>%
    filter(!is.na(change)) %>%
    ungroup()
  
  to_plot_2 <-
    to_plot %>%
    group_by(LABEL) %>%
    arrange(LABEL, -treat) %>%
    mutate(diff_in_diff = change - lag(change)) %>%
    filter(!is.na(diff_in_diff))
  
  p1 <- ggplot(data=to_plot_2) +
    geom_point(
      aes(x=forcats::fct_reorder(factor(LABEL), hd_area_acres), 
          y=diff_in_diff, 
          size=hd_area_acres
      )
    ) +
    xlab("") +
    ylab("") +
    coord_flip() +
    ggdark::dark_theme_gray()
  
  p2 <- ggplot(data=to_plot) +
    geom_point(
      aes(x=forcats::fct_reorder(factor(LABEL), hd_area_acres), 
          y=change, 
          color=forcats::fct_rev(factor(treat)),
          size=hd_area_acres
      )
    ) +
    coord_flip() +
    ggdark::dark_theme_gray()
  
  attgt <- did::att_gt(yname = dep_var,
                       gname = "did_post",
                       idname = "did_unique_id",
                       tname = "year",
                       xformla = ~1,
                       data =  buffer_ts, 
                       clustervars = cvars,
                       weightsname="n_tot",
                       allow_unbalanced_panel = T,
                       base_period = "varying",
                       panel = F
  )
  
  if (return_shps) {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts,
                "outer_buffer"=outer_buffer, 'inner_buffer'=inner_buffer,
                'nohd_blocks'=nohd_blocks, 'hd_blocks'=hd_blocks))
  } else {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts))
  }
}


hds_to_omit <-
  c("Emerald St", 
    "Emerald St HD",
    "Grant Rd", 
    "Grant Circle",
    "Union Market",
    "Lafayette Square",
    "Grant Rd HD", 
    "Mount Vernon Triangle HD",
    "Mount Vernon Triangle",
    "Pennsylvania Ave NHS HD",
    "Pennsylvania Ave NHS",
    "Grant Circle HD", "Union Market HD",
    # "Downtown",
    # "Downtown HD",
    "Lafayette Square HD")


res <- do_buffer_analysis(buffer_size = 100, 
                   hds_to_omit = hds_to_omit, 
                   cvars = "LABEL", 
                   dep_var = "pop_density")

summary(res[['attgt']])
group_effects <- aggte(res[['attgt']], type = "simple", na.rm = T)
summary(group_effects)

group_effects <- aggte(res[['attgt']], type = "dynamic", na.rm = T)
summary(group_effects)
ggdid(group_effects) +
  ggdark::dark_theme_gray()


res[['p1']]

res <- do_buffer_analysis(buffer_size = 800, 
                          hds_to_omit = hds_to_omit, 
                          cvars = "LABEL", 
                          dep_var = "pop_density")

summary(res[['attgt']])
group_effects <- aggte(res[['attgt']], type = "simple", na.rm = T)
summary(group_effects)

group_effects <- aggte(res[['attgt']], type = "dynamic", na.rm = T)
summary(group_effects)
ggdid(group_effects) +
  ggdark::dark_theme_gray()













test <- 
  dplyr::bind_rows(hd_blocks, nohd_blocks) %>%
  sf::st_drop_geometry(.) %>%
  # create our main variables of interest
  mutate(pop_density = n_tot / as.vector(geo_area_meters),
         pct_black = n_black / n_tot,
         pct_white = n_white / n_tot) %>%
  # create some indicator and ID variables we'll need
  mutate(post = ifelse(year > desig_decade, 1, 0),
         did_post = ifelse(treat==1, desig_decade+10, 0)) %>%
  group_by(year, LABEL, treat, geo_id) %>%
  mutate(did_unique_id = cur_group_id()) %>%
  ungroup() %>%
  select(geo_id, did_unique_id, LABEL, year, treat, post, did_post, 
         desig_decade, pop_density, n_black, n_white, n_tot, 
         pct_black, pct_white) %>%
  # get the population density
  mutate(pop_density = pop_density * 4046.86) %>%
  # again, remove smaller blocks (I am paranoid lol)
  filter(n_tot > 10) %>%
  # get a "relative year" variable, which is 0 in the decade the HD was designated
  mutate(rel_year = (year - desig_decade) / 10) %>%
  # merge on the # of acres in the HD
  dplyr::left_join(y = hd_shp %>% 
                     mutate(hd_area_acres = as.vector(sf::st_area(.)) / 4046.86 ) %>%
                     sf::st_drop_geometry(.) %>% 
                     select(LABEL, hd_area_acres),
                   by = "LABEL")

# remove blocks that are over 80% unzoned land
overlap_i <- 
  sf::st_intersection(x = dplyr::bind_rows(hd_blocks, nohd_blocks), y = sf::st_union(unzoned_shp)) %>% 
  mutate(overlap_area = as.vector(sf::st_area(.))) %>%
  sf::st_drop_geometry(.) %>%
  group_by(geo_id) %>%
  summarize(overlap_area = sum(overlap_area, na.rm = T)) %>%
  dplyr::right_join(y = buffer_ts, by='geo_id') %>%
  mutate(pct_overlap = overlap_area / as.vector(geo_area_meters),
         threshold   = ifelse(pct_overlap >= .8, 1, 0))

blocks_w_mostly_unzoned_land <- unique(overlap_i$geo_id[overlap_i$threshold==1 & !is.na(overlap_i$threshold)])
buffer_ts <- 
  filter(buffer_ts, !(geo_id %in% blocks_w_mostly_unzoned_land))




res200 <- do_buffer_analysis(buffer_size = 200, 
                             hds_to_omit = hds_to_omit, 
                             cvars = "LABEL", 
                             dep_var = "pop_density", 
                             return_shps = T)

# Restrict to pre-treatment periods only
pre_treatment_data <- res200$buffer_ts %>%
  filter(post==0)

# Test for differential pre-trends
pre_trend_test <- lm(
  pct_black ~ year * treat,
  data = pre_treatment_data
)

summary(pre_trend_test)
cat("\n=== PRE-TREND TEST ===\n")
cat("Null hypothesis: Pre-treatment trends are parallel\n")
cat("Test: Coefficient on period:treated interaction\n\n")

pre_coef <- summary(pre_trend_test)$coefficients["year:treat", ]
cat(sprintf("Coefficient: %.4f\n", pre_coef["Estimate"]))
cat(sprintf("Std. Error: %.4f\n", pre_coef["Std. Error"]))
cat(sprintf("p-value: %.4f\n", pre_coef["Pr(>|t|)"]))

if (pre_coef["Pr(>|t|)"] < 0.05) {
  cat("\nWARNING: Parallel trends assumption may be violated (p < 0.05)\n")
} else {
  cat("\nParallel trends assumption supported (p >= 0.05)\n")
}

did_model <- feols(
  pop_density ~ treat * post | LABEL + year,
  data = res200$buffer_ts
)

summary(did_model)

mod <- feols(pop_density ~ sunab(did_post, year) | LABEL + year,
      data = res200$buffer_ts,
      cluster = ~LABEL)
summary(mod, agg = "ATT")





















































#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
#'
#' @param ... Parameters to pass to the relevant method.
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param es Result from aggte (object of class AGGTEobj).
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @param gridPoints Number of grid points used for the underlying test
#'  inversion. Default equals 100. User may wish to change the number of grid
#'  points for computational reasons.
#' @param ... Parameters to pass to `createSensitivityResults` or
#'  `createSensitivityResults_relativeMagnitudes`.
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  type <- match.arg(type)
  
  # Make sure that user is passing in an event study
  if (es$type != "dynamic") {
    stop("need to pass in an event study")
  }
  
  # Check if used universal base period and warn otherwise
  if (es$DIDparams$base_period != "universal") {
    stop("Use a universal base period for honest_did")
  }
  
  # Recover influence function for event study estimates
  es_inf_func <- es$inf.function$dynamic.inf.func.e
  
  # Recover variance-covariance matrix
  n <- nrow(es_inf_func)
  V <- t(es_inf_func) %*% es_inf_func / n / n
  
  # Check time vector is consecutive with referencePeriod = -1
  referencePeriod <- -1
  consecutivePre  <- !all(diff(es$egt[es$egt <= referencePeriod]) == 1)
  consecutivePost <- !all(diff(es$egt[es$egt >= referencePeriod]) == 1)
  if ( consecutivePre | consecutivePost ) {
    msg <- "honest_did expects a time vector with consecutive time periods;"
    msg <- paste(msg, "please re-code your event study and interpret the results accordingly.", sep="\n")
    stop(msg)
  }
  
  # Remove the coefficient normalized to zero
  hasReference <- any(es$egt == referencePeriod)
  if ( hasReference ) {
    referencePeriodIndex <- which(es$egt == referencePeriod)
    V    <- V[-referencePeriodIndex,-referencePeriodIndex]
    beta <- es$att.egt[-referencePeriodIndex]
  } else {
    beta <- es$att.egt
  }
  
  nperiods <- nrow(V)
  npre     <- sum(1*(es$egt < referencePeriod))
  npost    <- nperiods - npre
  if ( !hasReference & (min(c(npost, npre)) <= 0) ) {
    if ( npost <= 0 ) {
      msg <- "not enough post-periods"
    } else {
      msg <- "not enough pre-periods"
    }
    msg <- paste0(msg, " (check your time vector; note honest_did takes -1 as the reference period)")
    stop(msg)
  }
  
  baseVec1 <- basisVector(index=(e+1),size=npost)
  orig_ci  <- constructOriginalCS(betahat        = beta,
                                  sigma          = V,
                                  numPrePeriods  = npre,
                                  numPostPeriods = npost,
                                  l_vec          = baseVec1)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             ...)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          ...)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}



test <-
  res100$buffer_ts %>% 
  filter(LABEL!="Financial") %>%
  mutate(did_post_rel = ifelse(treat==1, (did_post - 1940) / 10, 0))

###
# Run the CS event-study with 'universal' base-period option
## Note that universal base period normalizes the event-time minus 1 coef to 0
cs_results <- did::att_gt(yname = "pop_density",
                          tname = "year_rel",
                          idname = "did_unique_id",
                          gname = "did_post_rel",
                          data = res800$buffer_ts %>% 
                            filter(LABEL!="Financial") %>%
                            mutate(year_rel = year / 10) %>%
                            mutate(did_post_rel = ifelse(treat==1, (did_post) / 10, 0)),
                          control_group = "nevertreated",
                          base_period = "universal",
                          xformla = ~1,
                          weightsname = "n_tot",
                          clustervars = "LABEL",
                          panel=F,
                          allow_unbalanced_panel = T
                          )

cs_results
es <- did::aggte(cs_results, 
                 min_e = -4, max_e = 4,
                 type = "dynamic", na.rm = T
                 )

ggdid(es, theme=F) + ggdark::dark_theme_gray()
es



































res100 <- do_buffer_analysis(buffer_size = 100, 
                             hds_to_omit = hds_to_omit, 
                             cvars = "LABEL", 
                             dep_var = "pop_density",
                             # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
                             # blocks that are more than half covered by unzoned area
                             return_shps = F,
                             bp="universal")

temp <- res100$buffer_ts


temp <-
  temp %>%
  mutate(year = year / 10,
         did_post = did_post / 10)

attgt_temp <- did::att_gt(yname = "pop_density",
                     gname = "did_post",
                     idname = "did_unique_id",
                     tname = "year",
                     xformla = ~1,
                     data =  temp %>% filter(LABEL != "Colony Hill"), 
                     clustervars = "LABEL",
                     weightsname="n_tot",
                     allow_unbalanced_panel = T,
                     base_period = "universal",
                     panel = F
)

simple_effect <- aggte(attgt_temp, type = "simple", na.rm = T)

dyn <- aggte(attgt_temp, type = "dynamic", balance_e = 3)

ggdid(dyn) 
#Run sensitivity analysis for relative magnitudes
sensitivity_results <-
  honest_did(dyn,
             # e=0,
             type= "relative_magnitude",
             Mbarvec=seq(from = 0.25, to = 2, by = 0.25), alpha=.1
             )

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci) +
  geom_hline(yintercept = 0, size=1, color="white", linetype=2) + ggdark::dark_theme_gray()


sensitivity_results <-
  honest_did(dyn,
             # e=0,
             type= "smoothness",
             # Mbarvec=seq(from = 0.25, to = 2, by = 0.25), 
             alpha=.1
  )

HonestDiD::createSensitivityPlot(sensitivity_results$robust_ci,
                                                    sensitivity_results$orig_ci) +
  geom_hline(yintercept = 0, size=1, color="white", linetype=2) + ggdark::dark_theme_gray()


# test <- haven::read_dta("https://raw.githubusercontent.com/Mixtape-Sessions/Advanced-DID/main/Exercises/Data/ehec_data.dta")
# View(test)
# 
# cs_results <- did::att_gt(yname = "dins",
#                           tname = "year",
#                           idname = "stfips",
#                           gname = "yexp2",
#                           data = df %>% mutate(yexp2 = ifelse(is.na(yexp2), 3000, yexp2)),
#                           control_group = "notyettreated",
#                           base_period = "universal")
















#' @description
#' This function takes a regression estimated using fixest with the sunab option
#' and extracts the aggregated event-study coefficients and their variance-covariance matrix
#' @param sunab_fixest The result of a fixest call using the sunab option
#' @returns A list containing beta (the event-study coefficients),
#'          sigma (the variance-covariance matrix), and
#'          cohorts (the relative times corresponding to beta, sigma)

sunab_beta_vcv <-
  function(sunab_fixest){
    
    ## The following code block extracts the weights on individual coefs used in
    # the fixest aggregation ##
    sunab_agg   <- sunab_fixest$model_matrix_info$sunab$agg_period
    sunab_names <- base::names(sunab_fixest$coefficients)
    sunab_sel   <- base::grepl(sunab_agg, sunab_names, perl=TRUE)
    sunab_names <- sunab_names[sunab_sel]
    if(!base::is.null(sunab_fixest$weights)){
      sunab_wgt <- base::colSums(sunab_fixest$weights * base::sign(stats::model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    } else {
      sunab_wgt <- base::colSums(base::sign(stats::model.matrix(sunab_fixest)[, sunab_names, drop=FALSE]))
    }
    
    #Construct matrix sunab_trans such that sunab_trans %*% non-aggregated coefs = aggregated coefs,
    sunab_cohorts <- base::as.numeric(base::gsub(base::paste0(".*", sunab_agg, ".*"), "\\2", sunab_names, perl=TRUE))
    sunab_mat     <- stats::model.matrix(~ 0 + base::factor(sunab_cohorts))
    sunab_trans   <- base::solve(base::t(sunab_mat) %*% (sunab_wgt * sunab_mat)) %*% base::t(sunab_wgt * sunab_mat)
    
    #Get the coefs and vcv
    sunab_coefs   <- sunab_trans %*% base::cbind(sunab_fixest$coefficients[sunab_sel])
    sunab_vcov    <- sunab_trans %*% sunab_fixest$cov.scaled[sunab_sel, sunab_sel] %*% base::t(sunab_trans)
    
    base::return(base::list(beta    = sunab_coefs,
                            sigma   = sunab_vcov,
                            cohorts = base::sort(base::unique(sunab_cohorts))))
  }



temp <- res100$buffer_ts
temp$year_binned <- cut(temp$year, breaks=c(1930, 1960, 1990, 2030))
temp <- temp %>%
  mutate(
    years_since_treatment = year - did_post,
    years_since_treatment = ifelse(is.infinite(years_since_treatment), NA, years_since_treatment),
    years_since_treatment = ifelse(years_since_treatment < 0, 0, years_since_treatment)
  )
# Run fixest with sunab
temp$did_post <- ifelse(temp$did_post==0, Inf, temp$did_post)
formula_sunab <- pop_density ~ sunab(did_post, year) | LABEL + year
res_sunab <- fixest::feols(formula_sunab, cluster="LABEL", data=temp %>% 
                             # filter(!(LABEL=="Georgetown" & year==2020)) %>%
                             filter(!(LABEL %in% c("Blagden Alley/Naylor Court",
                                                   "Massachusetts Ave"
                                                   )))
                           )
fixest::iplot(res_sunab)

summary(res_sunab, agg="ATT")

# Extract the beta and vcv
beta_vcv <- sunab_beta_vcv(res_sunab)

# Run sensitivity analysis for relative magnitudes
kwargs <- list(betahat        = beta_vcv$beta,
               sigma          = beta_vcv$sigma,
               numPrePeriods  = sum(beta_vcv$cohorts < 0),
               numPostPeriods = sum(beta_vcv$cohorts > -1))
extra <- list(Mbarvec=seq(from = 0.5, to = 2, by = 0.5), gridPoints=100)

original_results <-
  do.call(HonestDiD::constructOriginalCS, kwargs)

sensitivity_results <-
  do.call(HonestDiD::createSensitivityResults_relativeMagnitudes,
          c(kwargs, extra))

HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity_results,
                                                    original_results)










attgt <- did::att_gt(yname = "pop_density",
                     gname = "did_post",
                     idname = "did_unique_id",
                     tname = "year",
                     xformla = ~1,
                     data =  res100$buffer_ts %>% filter(LABEL != "Colony Hill") %>% filter(rel_year >= -5), 
                     clustervars = "LABEL",
                     weightsname="n_tot",
                     allow_unbalanced_panel = T,
                     base_period = "varying",
                     panel = F
)

summary(attgt)
group_effects <- aggte(attgt, type = "dynamic", na.rm=T, alp=.1)

ggdid(group_effects) + ggdark::dark_theme_gray()






















make_plots <- function(myb) {
  
  res50 <- do_buffer_analysis_w_max_extent(buffer_size = 50, 
                                           hds_to_omit = hds_to_omit, 
                                           cvars = "LABEL", 
                                           dep_var = "pop_density",
                                           max_years_back = myb,
                                           only_extend_outer_buffer = T,
                                           subset_to_smaller_blocks=T,
                                           # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
                                           # blocks that are more than half covered by unzoned area
                                           return_shps = F)
  
  # res100 <- do_buffer_analysis_w_max_extent(buffer_size = 100, 
  #                                           hds_to_omit = hds_to_omit, 
  #                                           cvars = "LABEL", 
  #                                           dep_var = "pop_density",
  #                                           max_years_back = myb,
  #                                           # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
  #                                           # blocks that are more than half covered by unzoned area
  #                                           return_shps = F)
  # 
  # res200 <- do_buffer_analysis_w_max_extent(buffer_size = 200, 
  #                                           hds_to_omit = hds_to_omit, 
  #                                           cvars = "LABEL", 
  #                                           dep_var = "pop_density",
  #                                           max_years_back = myb,
  #                                           # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
  #                                           # blocks that are more than half covered by unzoned area
  #                                           return_shps = F)
  # 
  # resmyb0 <- do_buffer_analysis_w_max_extent(buffer_size = myb0, 
  #                                           hds_to_omit = hds_to_omit, 
  #                                           cvars = "LABEL", 
  #                                           dep_var = "pop_density",
  #                                           max_years_back = myb,
  #                                           # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
  #                                           # blocks that are more than half covered by unzoned area
  #                                           return_shps = F)
  
  res800 <- do_buffer_analysis_w_max_extent(buffer_size = 800, 
                                            hds_to_omit = hds_to_omit, 
                                            cvars = "LABEL", 
                                            dep_var = "pop_density",
                                            max_years_back = myb,
                                            only_extend_outer_buffer = T,
                                            subset_to_smaller_blocks=T,
                                            # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
                                            # blocks that are more than half covered by unzoned area
                                            return_shps = F)
  
  # res800 <- do_buffer_analysis_w_max_extent(buffer_size = 1000, 
  #                                           hds_to_omit = hds_to_omit, 
  #                                           cvars = "LABEL", 
  #                                           dep_var = "pop_density",
  #                                           max_years_back = myb,
  #                                           # unzoned_threshold = .5,  # uncomment this line to rerun the analysis, omitting
  #                                           # blocks that are more than half covered by unzoned area
  #                                           return_shps = F)
  
  
  
  summarize_buffer <- function(buf_df, dist) {
    rv <- 
      buf_df %>%
      group_by(LABEL, treat, post) %>%
      summarise(pop_density = mean(pop_density, na.rm=T),
                pct_black   = mean(pct_black, na.rm=T),
                pct_white   = mean(pct_white, na.rm=T),
                n_tot       = sum(n_tot, na.rm=T),
                
                hd_area_acres = max(hd_area_acres, na.rm=T),
                shp_area       = sum(as.vector(geo_area_meters), na.rm=T)
      ) %>%
      mutate(dist = ifelse(post==1, dist, -1*dist))
    
    return(rv)
  }
  
  
  my_plt <-
    dplyr::bind_rows(
      summarize_buffer(res50$buffer_ts, 50),
      summarize_buffer(res800$buffer_ts, 800),
    ) %>%
    ungroup() %>%
    group_by(treat, post, dist) %>%
    summarise(
      dist = mean(dist),
      pop_density = weighted.mean(pop_density, w = hd_area_acres, na.rm=T),
      pct_black = weighted.mean(pct_black, w = hd_area_acres, na.rm=T),
      pct_white = weighted.mean(pct_white, w = hd_area_acres, na.rm=T),
      
      total_area = sum(shp_area, na.rm = T)
    )
  
  temp <-
    ggplot(my_plt) +
    geom_line(
      aes(
        x=dist, 
        group=as.factor(paste(treat, post)), 
        y=pop_density, 
        color=forcats::fct_rev(as.factor(treat))
      ),
      linewidth = 1
    ) +
    theme(legend.position = "top") +
    ggdark::dark_theme_gray() +
    ylab("mean ppl/acre") + xlab("<- dist. (m) from HD border before desig. | dist. (m) from HD border after desig. ->") +
    scale_color_manual(values=c("#F8766D", "#00BFC4"), labels=c("In HD", "Outside HD"), name="") +
    ggtitle("After designation, population density increased more outside of HDs") +
    ylim(c(0, max(my_plt$pop_density, na.rm=T)))
  
  
  print("________________________________________________________________")
  print(paste("myb is", myb))
  print(
    res50$buffer_ts %>%
    group_by(treat, post) %>%
    summarize(area = sum(as.vector(geo_area_meters)))
  )
  
  print(temp)
  
  return(my_plt)
  
}


test1 <- make_plots(myb = 40)
# test2 <- make_plots(myb = 30)
test3 <- make_plots(myb = 20)
# test4 <- make_plots(myb = 10)



test1 %>% group_by(treat, post) %>%
  summarize(area = sum(as.vector(geo_area_meters)))
