
# This file contains functions we'll use to clean and analyze the data
library(here)
library(did)
library(haven)
library(fixest)
library(HonestDiD)




#' ⢀⣸ ⢀⣀ ⣰⡀ ⢀⣀   ⢀⣀ ⡇ ⢀⡀ ⢀⣀ ⣀⡀ ⠄ ⣀⡀ ⢀⡀   ⣰⡁ ⡀⢀ ⣀⡀ ⢀⣀ ⣰⡀ ⠄ ⢀⡀ ⣀⡀ ⢀⣀
#' ⠣⠼ ⠣⠼ ⠘⠤ ⠣⠼   ⠣⠤ ⠣ ⠣⠭ ⠣⠼ ⠇⠸ ⠇ ⠇⠸ ⣑⡺   ⢸  ⠣⠼ ⠇⠸ ⠣⠤ ⠘⠤ ⠇ ⠣⠜ ⠇⠸ ⠭⠕

quietly <- function(input) {
  # helper function to suppress messages and warnings so the HTML file 
  # knits nicely
  return(suppressWarnings(suppressMessages(input)))
}


# load & clean census tract data
# please see https://opendata.dc.gov/datasets/DCGIS::census-tracts-in-1970/about
# for example, for more details on variable names
load_clean_tracts <- function(geo_id_var, black_var, white_var, totpop_var, year) {
  # Function parameters:
  # geo_id_var: name of the column containing geographic identifiers (as string)
  # black_var: name of the column containing Black population count (as string)
  # white_var: name of the column containing White population count (as string)
  # totpop_var: name of the column containing total population count (as string)
  # year: the year of the census data (used in file path and as metadata)
  
  # Load the tract shapefile
  shp <- sf::st_read(paste0("tract_data/Census_Tracts_in_", year, 
                            "/Census_Tracts_in_", year, ".shp"), quiet=T) 
  # Clean and standardize the data
  shp <- shp %>% 
    # Rename columns to standardized names using non-standard evaluation
    # !!sym() converts string column names to symbols for use in dplyr
    rename("geo_id" = !!sym(geo_id_var),
           "n_black" = !!sym(black_var),
           "n_white" = !!sym(white_var),
           "n_tot" = !!sym(totpop_var)
    ) %>%
    # Keep only the geo_id columns and any other columns starting with "n_"
    select("geo_id", starts_with("n_")) %>%
    # Calculate derived variables
    mutate(n_other = n_tot - (n_black + n_white),
           year = year)
  # Transform coordinate reference system
  shp <- sf::st_transform(shp, 26918)
  # Calculate area of each tract in square meters
  # st_area() returns area in the units of the coordinate system
  shp$geo_area_meters <- sf::st_area(shp)
  
  # Return cleaned dataset with standardized column order
  # Includes: year, geographic ID, population counts by race, area, and geometry
  return(shp %>% select("year", "geo_id", "n_tot", "n_black", "n_white", "n_other", "geo_area_meters", "geometry"))
}

# Clean and merge census block geographic and demographic data
clean_block_data <- function(shp, df, shp_b_id, df_b_id, var_prefix, df_n_black, df_n_white, drop_var="", year) {
  # Function parameters:
  # shp: spatial dataframe (sf object) containing block geometries
  # df: regular dataframe containing demographic data
  # shp_b_id: name of the block ID column in the spatial dataframe (as string)
  # df_b_id: name of the block ID column in the demographic dataframe (as string)
  # var_prefix: prefix string to identify population count columns in df
  # df_n_black: name of the Black population column in df (as string)
  # df_n_white: name of the White population column in df (as string)
  # drop_var: optional column name to remove from df (default: empty string)
  # year: year of the census data (added as metadata)
  
  # Clean the spatial dataframe
  # Keep only the block ID column and rename it to standard "geo_id"
  shp <- shp %>% select(!!sym(shp_b_id)) %>% rename("geo_id" = !!sym(shp_b_id))
  # Optional: remove unwanted column from demographic dataframe
  # Only executes if drop_var parameter is not empty
  if (drop_var!="") {df <- df %>% select(-!!sym(drop_var))}
  
  # Process demographic dataframe
  df <- df %>% 
    # Keep block ID and all columns starting with the specified prefix
    select(!!sym(df_b_id), starts_with(var_prefix)) %>%
    # Apply rowwise operations to calculate totals for each row
    rowwise() %>%
    # Sum all columns with the specified prefix to get total population
    # c_across() selects multiple columns for the sum() function
    mutate(n_tot = sum(c_across(starts_with(var_prefix))))
  
  # Further clean the demographic data
  df <- df %>%
    # Rename columns to standardized names
    rename("geo_id" = !!sym(df_b_id),
           "n_black" = !!sym(df_n_black),
           "n_white" = !!sym(df_n_white)) %>%
    # Remove the original prefix columns (no longer needed after summing)
    select(-starts_with(var_prefix)) %>%
    # Calculate population of other races/ethnicities
    mutate(n_other = n_tot - (n_black + n_white))
  
  # Merge spatial and demographic data
  # Left join ensures all geographic units are retained
  shp <- dplyr::left_join(shp, df, by="geo_id") 
  # Transform coordinate reference system
  shp <- sf::st_transform(shp, 26918)
  # Calculate area of each block in square meters
  shp$geo_area_meters <- sf::st_area(shp)
  # Add year as metadata column
  shp$year <- year
  
  # Return the cleaned and merged spatial dataframe
  return(shp %>% select("year", "geo_id", "n_tot", "n_black", "n_white", "n_other", "geo_area_meters", "geometry"))
}


# fix broken geometries, if there are problems
fix_geo_if_broken <- function(shp, verbose=F) {
  if (min(sf::st_is_valid(shp)) == 0) {
    if (verbose) {print("Fixing geometry...")}
    return(sf::st_make_valid(shp))
  } else {
    return(shp)
  }
}





# clean block group data
clean_blockgroup_data <- function(shp, year) {
  # This function cleans the block group data
  #' @param shp: the shapefile
  #' @param year: the year of data the shapefile represents
  #' @return: returns the cleaned shapefile 
  if (year==2010) {
    shp <- 
      shp %>% mutate(across(starts_with("J"), ~ as.numeric(.x))) %>%
      mutate(
        median_age_ = JL0E001,             # median age
        tot_pop_ = JMJE001,                # race/eth: total population
        n_white_ = JMJE003,                # race/eth: not hispanic white alone
        n_black_ = JMJE004,                # race/eth: not hispanic black alone
        n_hispa_ = JMJE012,                # race/eth: hispanic or latino
        tot_commute_mode_ = JM0E001,       # Means of Transportation to Work: universe pop
        n_drove_alone_ = JM0E003,          # drove alone to work
        n_walked_to_work_ = JM0E019,       # walked to work
        tot_travel_time_ = JM2E001,        # total travel time
        n_travel_time_lt_20_ = JM2E002 +   # travel time less than 20 mins
                               JM2E003 +
                               JM2E004 +
                               JM2E005,     
        tot_marital_status_ = JNXE001,     # total marital status
        n_married_ = JNXE004 + JNXE013,    # number of married people
        tot_education_ = JN9E001,          # total educational attainment
        n_post_bach_ = JN9E016 +           # number of ppl w more than bachelors degree
                      JN9E017 +
                      JN9E018 +
                      JN9E033 +
                      JN9E034 +
                      JN9E035,
        tot_pov_status_ = JOFE001,         # total poverty
        n_in_pov_ = JOFE002,               # number HH in poverty
        median_hhinc_ = JO3E001,           # median HH income
        tot_profession_ = JRGE001,         # total people for industry/profession groups
        n_service_sector_ = JRGE021 +      # number of ppl in service jobs
                           JRGE024 +
                           JRGE027 +
                           JRGE048 +
                           JRGE051 +
                           JRGE054,
        n_professional_sector_ = JRGE017 + # number of ppl in "professional" jobs
                                JRGE044,
        tot_housing_units_ = JSDE001,      # total number of housing units
        median_year_str_built_ = JSEE001,  # median year the housing structure was built
        tot_car_free_ = JSNE001,           # total for car ownership variables
        n_car_free_ = JSNE003 + JSNE010,   # num HH w/ no car
        median_home_val_ = JTIE001,        # median home value, owner-occupied housing units
        tot_rent_30_ = JTBE001 - JTBE011,  # demoninator for # of HH paying 30%+ of income in rent
        n_rent_30_ = JTBE007 + 
                     JTBE008 +
                     JTBE009 +
                     JTBE010
      ) %>%
      select(GISJOIN, ends_with("_"))
  } 
  if (year==2023) {
    shp <- 
      shp %>% mutate(across(starts_with("A"), ~ as.numeric(.x))) %>%
      mutate(
        median_age_ = ASNRE001,             # median age
        tot_pop_ = ASOAE001,                # race/eth: total population
        n_white_ = ASOAE003,                # race/eth: not hispanic white alone
        n_black_ = ASOAE004,                # race/eth: not hispanic black alone
        n_hispa_ = ASOAE012,                # race/eth: hispanic or latino
        tot_commute_mode_ = ASORE001,       # Means of Transportation to Work: universe pop
        n_drove_alone_ = ASORE003,          # drove alone to work
        n_walked_to_work_ = ASORE019,       # walked to work
        tot_travel_time_ = ASOTE001,        # total travel time
        n_travel_time_lt_20_ = ASOTE002 +   # travel time less than 20 mins
          ASOTE003 +
          ASOTE004 +
          ASOTE005,     
        tot_marital_status_ = ASPPE001,     # total marital status
        n_married_ = ASPPE004 + ASPPE013,   # number of married people
        tot_education_ = ASP2E001,          # total educational attainment
        n_post_bach_ = ASP2E016 +           # number of ppl w more than bachelors degree
          ASP2E017 +
          ASP2E018 +
          ASP2E033 +
          ASP2E034 +
          ASP2E035,
        tot_pov_status_ = ASQLE001,         # total poverty
        n_in_pov_ = ASQLE002,               # number HH in poverty
        median_hhinc_ = ASQ1E001,           # median HH income
        tot_profession_ = ASS5E001,         # total people for industry/profession groups
        n_service_sector_ = ASS5E021 +      # number of ppl in service jobs
          ASS5E024 +
          ASS5E027 +
          ASS5E048 +
          ASS5E051 +
          ASS5E054,
        n_professional_sector_ = ASS5E017 +  # number of ppl in "professional" jobs
          ASS5E044,
        tot_housing_units_ = ASS7E001,      # total number of housing units
        median_year_str_built_ = ASUKE001,  # median year the housing structure was built
        tot_car_free_ = ASUTE001,           # total for car ownership variables
        n_car_free_ = ASUTE003 + ASUTE010,  # num HH w/ no car
        median_home_val_ = ASVNE001,        # median home value, owner-occupied housing units
        tot_rent_30_ = ASVHE001 - ASVHE011, # demoninator for # of HH paying 30%+ of income in rent
        n_rent_30_ = ASVHE007 + 
          ASVHE008 +
          ASVHE009 +
          ASVHE010
      ) %>%
      select(GISJOIN, ends_with("_")) %>%
      mutate(across(ends_with("_"), ~ case_when(. < 0 ~ NA_real_, TRUE ~ .)))
  }
  
  return(shp)
}



summarize_bg_data <- function(bg_shp, group_vars) {
  # Summarizes the block group data, creating a long format data set that
  # just has each variable, whether or not the row corresponds to a historic district,
  # and the variable's value
  #' @param bg_shp: a block group shapefile with data
  #' @return to_plot: the long format data file summarized by "in HDs" vs not

  to_plot <-
    bg_shp %>%
    sf::st_drop_geometry() %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(across(starts_with("n_"), sum, na.rm=T), 
              median_age_      = matrixStats::weightedMedian(x = median_age_, w = tot_pop_, na.rm=T),
              median_hhinc_    = matrixStats::weightedMedian(x = median_hhinc_, w = tot_pov_status_, na.rm=T),
              median_home_val_ = matrixStats::weightedMedian(x = median_home_val_, w = tot_housing_units_, na.rm=T),
              median_year_str_built_ = matrixStats::weightedMedian(median_year_str_built_, w = tot_housing_units_, na.rm=T),
              across(starts_with("tot_"), sum, na.rm=T)
    ) %>%
    ungroup() %>%
    group_by(!!!syms(group_vars)) %>%
    summarise(
      median_age      = mean(x = median_age_, na.rm=T),
      median_hhinc    = mean(x = median_hhinc_, na.rm=T),
      median_home_val = mean(x = median_home_val_, na.rm=T),
      median_year_str_built = mean(median_year_str_built_, na.rm=T),
      
      pct_rent_over30 = n_rent_30_ / tot_rent_30_,
      
      pct_white = n_white_ / tot_pop_,
      pct_black = n_black_ / tot_pop_,
      pct_hispa = n_hispa_ / tot_pop_,
      pct_other = (tot_pop_ - n_white_ - n_black_ - n_hispa_) / tot_pop_,
      
      pct_drive = n_drove_alone_ / tot_commute_mode_,
      pct_walk = n_walked_to_work_ / tot_commute_mode_,
      pct_carfree = n_car_free_ / tot_car_free_,
      pct_20mins_or_less = n_travel_time_lt_20_ / tot_travel_time_,
      
      pct_married = n_married_ / tot_marital_status_,
      pct_mt_bach = n_post_bach_ / tot_education_,
      pct_professional_sector = n_professional_sector_ / tot_profession_,
      pct_service_sector = n_service_sector_ / tot_profession_,
      pct_in_pov = n_in_pov_ / tot_pov_status_
    ) %>%
    tidyr::pivot_longer(
      cols = -matches(group_vars), # Exclude the grouping variable(s)
      names_to = "variable",
      values_to = "value"
    )
  
  to_plot_district_total <-
    bg_shp %>%
    sf::st_drop_geometry() %>%
    summarise(
      median_age      = matrixStats::weightedMedian(x = median_age_, w = tot_pop_, na.rm=T),
      median_hhinc    = matrixStats::weightedMedian(x = median_hhinc_, w = tot_pov_status_, na.rm=T),
      median_home_val = matrixStats::weightedMedian(x = median_home_val_, w = tot_housing_units_, na.rm=T),
      median_year_str_built = matrixStats::weightedMedian(median_year_str_built_, w = tot_housing_units_, na.rm=T),
      
      pct_rent_over30 = sum(n_rent_30_, na.rm=T) / sum(tot_rent_30_, na.rm=T),
      
      pct_white = sum(n_white_, na.rm=T) / sum(tot_pop_, na.rm=T),
      pct_black = sum(n_black_, na.rm=T) / sum(tot_pop_, na.rm=T),
      pct_hispa = sum(n_hispa_, na.rm=T) / sum(tot_pop_, na.rm=T),
      pct_other = sum(tot_pop_ - n_white_ - n_black_ - n_hispa_, na.rm=T) / sum(tot_pop_, na.rm=T),
      
      pct_drive = sum(n_drove_alone_, na.rm=T) / sum(tot_commute_mode_, na.rm=T),
      pct_walk = sum(n_walked_to_work_, na.rm=T) / sum(tot_commute_mode_, na.rm=T),
      pct_carfree = sum(n_car_free_, na.rm=T) / sum(tot_car_free_, na.rm=T),
      pct_20mins_or_less = sum(n_travel_time_lt_20_, na.rm=T) / sum(tot_travel_time_, na.rm=T),
      
      pct_married = sum(n_married_, na.rm=T) / sum(tot_marital_status_, na.rm=T),
      pct_mt_bach = sum(n_post_bach_, na.rm=T) / sum(tot_education_, na.rm=T),
      pct_professional_sector = sum(n_professional_sector_, na.rm=T) / sum(tot_profession_, na.rm=T),
      pct_service_sector = sum(n_service_sector_, na.rm=T) / sum(tot_profession_, na.rm=T),
      pct_in_pov = sum(n_in_pov_, na.rm=T) / sum(tot_pov_status_, na.rm=T)
    ) %>%
    mutate(LABEL="Entire District") %>%
    tidyr::pivot_longer(
      cols=-LABEL,
      names_to = "variable",
      values_to = "value"
    )
  
  return(dplyr::bind_rows(to_plot, to_plot_district_total))
}




## load rent breakdown data
load_rent_breakdown_data <- function() {
  rb <- readr::read_csv("block_group_data/nhgis0100_ds267_20235_blck_grp.csv", # load rent breakdown data
                        show_col_types = F) %>%
    mutate(
           tot_lt10k   = ASVKE002 - ASVKE010,
           n_lt10k_b   = ASVKE006 + ASVKE007 + ASVKE008 + ASVKE009,
           
           tot_10_19k  = ASVKE011 - ASVKE019,
           n_10_19k_b  = ASVKE015 + ASVKE016 + ASVKE017 + ASVKE018,
           
           tot_20_34k  = ASVKE020 - ASVKE028,
           n_20_34k_b  = ASVKE024 + ASVKE025 + ASVKE026 + ASVKE027,
           
           tot_35_49k  = ASVKE029 - ASVKE037,
           n_35_49k_b  = ASVKE033 + ASVKE034 + ASVKE035 + ASVKE036,
           
           tot_50_74k  = ASVKE038 - ASVKE046,
           n_50_74k_b  = ASVKE042 + ASVKE043 + ASVKE044 + ASVKE045,
             
           tot_75_99k  = ASVKE047 - ASVKE055,
           n_75_99k_b  = ASVKE051 + ASVKE052 + ASVKE053 + ASVKE054,
           
           tot_100k  = ASVKE056 - ASVKE064,
           n_100k_b  = ASVKE060 + ASVKE061 + ASVKE062 + ASVKE063,
           
           tot_lt100k = tot_lt10k + tot_10_19k + tot_20_34k + tot_35_49k + tot_50_74k + tot_75_99k,
           n_lt100k_b = n_lt10k_b + n_10_19k_b + n_20_34k_b + n_35_49k_b + n_50_74k_b + n_75_99k_b,
           
           pct_lt100k_b = n_lt100k_b / tot_lt100k,
           pct_100k_b   = n_100k_b   / tot_100k,
           
           tot_lt75k = tot_lt10k + tot_10_19k + tot_20_34k + tot_35_49k + tot_50_74k,
           n_lt75k_b = n_lt10k_b + n_10_19k_b + n_20_34k_b + n_35_49k_b + n_50_74k_b,
           
           tot_mt75k = tot_75_99k + tot_100k,
           n_mt75k_b = n_75_99k_b + n_100k_b,
           
           pct_lt75k_b = n_lt75k_b / tot_lt75k,
           pct_75k_b   = n_mt75k_b   / tot_mt75k
           )
  
  rv <- 
    left_join(x = bg_23_shp, y = rb, by = "GISJOIN") 
  
  return(rv)
}






# ⢀⣀ ⣀⡀ ⢀⣀ ⣰⡀ ⠄ ⢀⣀ ⡇   ⠄ ⣀⡀ ⣰⡀ ⢀⡀ ⡀⣀ ⢀⣀ ⢀⡀ ⢀⣀ ⣰⡀ ⠄ ⢀⡀ ⣀⡀   ⣰⡁ ⡀⢀ ⣀⡀ ⢀⣀ ⣰⡀ ⠄ ⢀⡀ ⣀⡀ ⢀⣀
# ⠭⠕ ⡧⠜ ⠣⠼ ⠘⠤ ⠇ ⠣⠼ ⠣   ⠇ ⠇⠸ ⠘⠤ ⠣⠭ ⠏  ⠭⠕ ⠣⠭ ⠣⠤ ⠘⠤ ⠇ ⠣⠜ ⠇⠸   ⢸  ⠣⠼ ⠇⠸ ⠣⠤ ⠘⠤ ⠇ ⠣⠜ ⠇⠸ ⠭⠕



# This function IDs which geographies in any given year are in HDs vs outside HDs:
get_geos_in_shp <- function(shp, min_pct, parent_shp, geo_id, parent_id) {
  # Function parameters:
  #' @param shp: the tract or block shapefile (an sf shapefile object) containing demographic data
  #' @param min_pct: minimum % of the tract/block that must be in the HD to count as part of the HD 
  #          (decimal between 0 and 1, e.g., 0.5 for 50%)
  #' @param parent_shp: the historic district shapefile
  #' @geo_id: the name of the geographic ID variable in the shp object
  #' @parent_id: the ids of the units of the parent_shp
  #' @return geos_in_hd: a dataframe with only goes in the parent shape, that has 
  #'                     two columns: the geo_id and the name of the parent shape
  
  # STEP 0: Get the area of each geo in shp
  shp$geo_area_meters <- sf::st_area(shp)
  
  # STEP 1: Spatial intersection analysis
  # Find the intersection between census units and historic districts
  # This creates new polygons where census units overlap with HDs
  i <- sf::st_intersection(x=shp, y=parent_shp)
  
  # Calculate the area of each intersection polygon in square meters
  i$i_area <- sf::st_area(i)
  
  # Calculate what percentage of the original census unit area falls within each HD
  # as.vector() removes units to get numeric values for comparison
  i$pct_of_geo_area <- as.vector(i$i_area / i$geo_area_meters)
  
  # STEP 2: Filter geographic units based on minimum percentage threshold
  # Keep only census units where a sufficient portion (> min_pct) falls within an HD
  # Select relevant columns for analysis
  geos_in_parent_shp <- i[i$pct_of_geo_area > min_pct,] %>% 
    sf::st_drop_geometry() %>%
    select(geo_id, parent_id) %>%
    distinct()
  
  return(geos_in_parent_shp)
}







# Function to crosswalk past years to a skeleton key year, so we have
# approximately consistent spatial units across time
create_xwalk <- function(other_year, root_year, shp) {
  #' @param other_year: the year we are xwalking from
  #' @param root_year: the root year we are xwalking to
  #' @param shp: the shapefile with all the polygons in all the years
  #' @returns rv: a data frame that has the older geo_ids, the newer geo_ids,
  #'              and the allocation percents to xwalk from the old to new geos
  
  # examples:
  # 1980_G11000100001204: this should have one match
  # 1980_G11000100066307: this should have two matches because it got split
  
  rv <- 
    sf::st_intersection(
      x = filter(shp, year==other_year), 
      y = filter(shp, year==root_year)
    ) %>%
    select(starts_with("geo_id"), starts_with("geo_area_")) %>%
    mutate(overlap_area = sf::st_area(.)) %>%
    sf::st_drop_geometry() %>%
    mutate(
      pct_from_in_to = units::drop_units(overlap_area / geo_area_meters),
      pct_to_in_from = units::drop_units(overlap_area / geo_area_meters.1)
    ) %>%
    arrange(geo_id, -pct_to_in_from) %>%
    # NB: geo_id here refers to the geo_id of the other year; geo_id.1 refers to
    # the geo_id of the root year
    group_by(geo_id) %>%
    # filter out cases where the mutual coverage is less than X%
    filter(! ((pct_from_in_to < .4) & (pct_to_in_from < .4))) %>%
    # create variable to indicate which to/root geo has the most overlap w/ the "from" geo
    mutate(max_pct_from_in_to = max(pct_from_in_to, na.rm=T)) %>%
    # filter out cases where the max % of the from geo (other year) in one of the to (root year) geos
    # is greater than X%, and the % from --> to coverage for *this* geo is less than Y% and the
    # % coverage of this geo on the "from"/past geo is less than Z%
    filter(!( (max_pct_from_in_to > .5) & (pct_from_in_to < .4) & (pct_to_in_from < .5)) ) %>%
    mutate(n_matches = n()) %>%
    # filter out cases where there's only one match but less than X%
    # of the root geo is covered by the other geo
    filter(! ((n_matches==1) & (pct_to_in_from < .2))) %>%
    # filter out cases where there's more than one match, but less than half of the
    # root geo is covered by the other geo, and less than 10% of the other geo is
    # covered by the new geo
    filter(! ((n_matches > 1) & (pct_to_in_from < .5) & (pct_from_in_to < 0.1)) ) %>%
    # also drop the reverse:
    filter(! ((n_matches > 1) & (pct_from_in_to < .5) & (pct_to_in_from < 0.1)) ) %>%
    # and drop anything where the % of the root geo in the other geo is less than X%
    filter(! (pct_to_in_from < .15)) %>%
    mutate(n_matches = n()) %>%
    # mutate(total_pct_from_in_to = sum(pct_from_in_to, na.rm=T)) %>%
    select(starts_with("geo_id"), starts_with("pct_"), n_matches) %>%
    rename(geo_id_from = geo_id,
           geo_id_to = geo_id.1) %>%
    # renormalize the percent_from_in_to values after eliminating spurious links
    group_by(geo_id_from) %>%
    mutate(pct_from_in_to_normed = pct_from_in_to / sum(pct_from_in_to, na.rm=T)) %>%
    ungroup()
  
  return(rv)
}



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






# ⣇⡀ ⡇ ⢀⡀ ⢀⣀ ⡇⡠   ⢀⣀ ⣀⡀ ⢀⣀ ⡇ ⡀⢀ ⢀⣀ ⠄ ⢀⣀   ⣰⡁ ⡀⢀ ⣀⡀ ⢀⣀ ⣰⡀ ⠄ ⢀⡀ ⣀⡀ ⢀⣀
# ⠧⠜ ⠣ ⠣⠜ ⠣⠤ ⠏⠢   ⠣⠼ ⠇⠸ ⠣⠼ ⠣ ⣑⡺ ⠭⠕ ⠇ ⠭⠕   ⢸  ⠣⠼ ⠇⠸ ⠣⠤ ⠘⠤ ⠇ ⠣⠜ ⠇⠸ ⠭⠕



get_closest_blocks <- function(shp, geos_in_hd, geos_outside_hds, y, hd, n) {
  #' @param shp: the blocks sf object
  #' @param geos_in_hd: a data frame that has the geo_ids in one HD we care about
  #' @param geos_outside_hds: a vector of geo_ids outside any HDs
  #' @param y: the current year, which should be the decennial year before HD creation
  #' @param hd: the HD of interest
  #' @param n: the number of nearby blocks to consider as matches for each HD block
  #' @returns closest_geo_ids: a data frame w/ 2 columns:
  #'                              one w/ the HD geo_id (repeated n times)
  #'                              and one w/ the geo_ids of the nearest n blocks outside HDs
  
  hd_blocks <- filter(geos_in_hd, (desig_decade == y) & (year==y)) %>% pull(geo_id)
  
  # calc distances using centroids to speed up the compute time
  hd_shp <- filter(shp, geo_id %in% hd_blocks) %>% sf::st_centroid(.)
  nohd_shp <- filter(shp, (geo_id %in% geos_outside_hds) & (year==y)) %>% sf::st_centroid(.)
  
  # calc distances
  dists <- sf::st_distance(hd_shp, nohd_shp)
  
  # get the indexes of the N closest blocks
  units(dists) <- NULL
  # make sure blocks can't match to themselves (distance would equal zero)
  dists[dists==0] <- NA
  # find the N closest blocks
  closest_indices <- t(apply(dists, 1, function(x) order(x)[1:n]))
  
  # create a data frame that has one col for the HD geo_id and another col for the
  # geo_id of the blocks outside of the HD that were closest to that in-HD block
  for (i in 1:nrow(hd_shp)) {
    temp_df <- 
      data.frame(
        hd_id   = rep(hd_shp$geo_id[i], n), 
        nohd_id = nohd_shp$geo_id[ closest_indices[i,] ] 
      )
    if (i==1) {
      closest_geo_ids <- temp_df
    } else {
      closest_geo_ids <- dplyr::bind_rows(closest_geo_ids, temp_df)
    }
  }
  
  return(closest_geo_ids)
}


get_root_year_data <- function(year, gid, shp, treat) {
  #' Grabs data from the root year (the last decennial year equal to or less 
  #' than the year in which the HD was created). 
  #' @param year: the root year we want data for. EX: Capitol Hill would be 1970.
  #' @param gid: the geo_id we want data for.
  #' @param shp: the shapefil with all the block info over all years.
  #' @param treat: 0 or 1; whether this blocks is a treatent (HD) or control
  #'               (non-HD block)
  #' @returns: returns the data for that block/geo_id, noting treatment group
  return(filter(shp, geo_id == gid) %>% mutate(treat=treat) %>% st_drop_geometry(.))
}


get_other_year_data <- function(y, root_gid, shp, xwalk, treat) {
  #' grabs data from non-root years. EX: 1960 for Capitol Hill.
  #' @param y: the year we're xwalking from. EX: 1950 or 1960 for Capitol Hill.
  #' @param root_gid: the geo_id of the root block we're working with
  #' @param shp: the shapefile with all the blocks over all years
  #' @param xwalk: the specific crosswalk file from year y to the root year
  #' @param treat: 0 or 1; whether this block is an HD block or a non-HD block
  #' @returns rv: the data associated w/ the block in year y
  
  #' get the geo_id in the "from" year associated with this root-year block
  #' the gid argument is the "to" gid, or the root year geo_id
  gid_other_y <-
    xwalk$geo_id_from[xwalk$geo_id_to==root_gid]
  #' join the xwalk to the data so we can allocate the data from the year y to 
  #' the root year. (the data lives in the shapefile)
  rv <- 
    dplyr::left_join(x  = shp %>% sf::st_drop_geometry(.) %>% 
                                  filter(geo_id %in% gid_other_y),
                     y  = xwalk, 
                     by = c("geo_id" = "geo_id_from")) %>%
    #' if there's only one "from" block that matches to the root year block,
    #' and more than 50% of the from block is covered by the to block,
    #' just allocate all of the data from the from block to the to block
    mutate(pct_from_in_to = 
             ifelse((n_matches == 1) & (pct_from_in_to>.5), 
                    1, 
                    pct_from_in_to)) %>%
    #' do the allocation of the data from year y to the root year:
    mutate(n_tot   = n_tot   * pct_from_in_to,
           n_black = n_black * pct_from_in_to,
           n_white = n_white * pct_from_in_to) %>%
    #' remove any rows where data was being allocated to a root-year block
    #' other than the one we're actually working with
    #' This happens when a block from year y xwalks to multiple blocks in the
    #' root year. But we're focused on one block in the root year at a time, 
    #' so we only want data for that one block.
    filter(geo_id_to == root_gid) %>%
    #' we're also going to combine data if multiple blocks in year y all
    #' xwalk to the root year block. We'll just take the sum.
    group_by(geo_id_to) %>%
    summarize(n_tot = sum(n_tot, na.rm=T),
              n_black = sum(n_black, na.rm=T),
              n_white = sum(n_white, na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = treat) %>%
    # rename(geo_id_from = geo_id) %>% TODO: check this doesn't break anything
    rename(geo_id = geo_id_to)
  
  return(rv)
}


create_matches <- function(n_nearest, min_year=1940, lookback = 80, matching_vars, screen_2020=T) {
  #' This function creates a data frame w/ two columns: one w the HD block ID,
  #' and one w/ the non-HD block ID that HD block got matched to.
  #' The purpose of the function is to match the HD blocks to the non-HD blocks
  #' such that their pre-HD-creation data are similar (e.g. similar
  #' population density)
  #' @param n_nearest: (integer) the number of nearest potential matching blocks
  #'                   to consider matching to a given HD block 
  #' @param min_year: (integer) the earliest year to use e.g. 1940
  #' @param lookback: (integer) # of years to go backwards for the match algo
  #' @param matching_vars: (vector of strings) the variables to use in the
  #'                       matching algorithm to do the matching on, 
  #'                       ex: c("n_tot_", "pop_density_", "n_white_")
  #' @param screen_2020: (bool) whether to remove potential matches that don't
  #'                     have population in 2020 
  #'
  #' @returns mb: (dataframe) the df w/ 2 cols: the hd block geo id, and the
  #'              geo id of the non-HD block it was matched to, all in the 
  #'              root year.
  
  # to loop through each historic district, we need a list of them:
  hd_list <- sort(unique(hd_shp$LABEL))
  
  # we'll create a simple data frame called "matched_blocks" (mb) that has
  # 2 columns: one for the HD block id and one for the non-HD block id it was
  # matched to
  mb <- data.frame(hd_id=NA, nohd_id=NA, LABEL=NA)
  
  if(VERBOSE) {print(paste(length(hd_list), "HDs"))}
  
  for (hd in hd_list) {
    if (hd %in% hds_to_omit) {next}
    if (VERBOSE) {cat(paste("\n\nHD is: ", hd))}
    
    # get the last decennial year before the HD was created:
    t0_year = unique(geos_in_hds$desig_decade[geos_in_hds$LABEL == hd])
    # and get the years before that t0 year:
    back_years = seq(min_year, ifelse(min_year==t0_year, t0_year, t0_year-10), 10)
    
    # get nearest N blocks to the block in the HD using the centroids of those blocks
    closest_blocks <-
      suppressWarnings(get_closest_blocks(shp = geos_shp, 
                                          geos_in_hd = filter(geos_in_hds, 
                                                              LABEL==hd & 
                                                                desig_decade==t0_year & 
                                                                year==t0_year), 
                                          geos_outside_hds = geos_outside_hds, 
                                          y = t0_year, hd = hd, n = n_nearest)
      )
    
    
    # add back year data, looping through each year:
    for (y in seq(min_year, t0_year, 10)) {
      # and through each set of HD and non-HD blocks:
      for (inhd_val in c(T, F)) {
        if (inhd_val & (y!=t0_year)) {
          cur_xwalk = inhd_xwalk_list[[paste0(y, "_", t0_year)]]
        } else if ((!inhd_val) & (y!=t0_year)) {
          cur_xwalk = nohd_xwalk_list[[paste0(y, "_", t0_year)]]
        } else {cur_xwalk=NA}
        
        closest_blocks <-
          add_back_year_data(closest_blocks = closest_blocks, 
                             geos_shp = geos_shp, 
                             inhd = inhd_val, 
                             xwalk =  cur_xwalk, 
                             back_year = y, 
                             t0_year = t0_year)
        
        # remove HD blocks that have no population or missing population
        closest_blocks <- 
          filter(closest_blocks, 
                 (!!sym(paste0("n_tot_", y, "_inhd")) > 0) & 
                   (!is.na(!!sym(paste0("n_tot_", y, "_inhd"))))
          )
        
      }
      # fix the fact that the function turns the df cols into lists:
      closest_blocks <- 
        closest_blocks %>% 
        tidyr::unnest(cols = everything()) %>%
        dplyr::left_join(y = filter(geos_shp, year==t0_year) %>% sf::st_drop_geometry(.) %>% select(geo_id, geo_area_meters),
                         by = c('hd_id'='geo_id')) %>% 
        rename(!!sym(paste0("area_", y, "_inhd")) := geo_area_meters) %>%
        dplyr::left_join(y = filter(geos_shp, year==t0_year) %>% sf::st_drop_geometry(.) %>% select(geo_id, geo_area_meters),
                         by = c('nohd_id'='geo_id')) %>% 
        rename(!!sym(paste0("area_", y, "_nohd")) := geo_area_meters) %>%
        mutate(!!sym(paste0("area_", y, "_inhd")) := units::drop_units(!!sym(paste0("area_", y, "_inhd"))),
               !!sym(paste0("area_", y, "_nohd")) := units::drop_units(!!sym(paste0("area_", y, "_nohd"))),
               !!sym(paste0("pop_dens_", y, "_inhd"))  := !!sym(paste0("n_tot_", y, "_inhd")) / !!sym(paste0("area_", y, "_inhd")),
               !!sym(paste0("pop_dens_", y, "_nohd"))  := !!sym(paste0("n_tot_", y, "_nohd")) / !!sym(paste0("area_", y, "_nohd")) 
               )
      # calculate normed variables by dividing each variable value by its max 
      # value in that year, then append that onto a dataset we're adding to
      # on each iteration:
      if (y==min_year) {
        closest_blocks_normed <- calc_normed_vals(cb = closest_blocks, year = y, t0_year)
      } else {
        closest_blocks_normed <- dplyr::full_join(closest_blocks_normed, 
                                                  calc_normed_vals(cb = select(closest_blocks, hd_id, nohd_id, contains(as.character(y)) ), 
                                                                   year = y, 
                                                                   t0_year),
                                                  by = c("hd_id", "nohd_id"))
      }
    }
    
    # closest_blocks_normed <- 
    #   select(closest_blocks_normed, ends_with("_id"), -starts_with("geo_area"), ends_with("_norm"))
    
    # find the best matching non-HD block for each HD block:
    if (hd=="Colony Hill") {temp_screen = F} else {temp_screen=screen_2020}
    matched_blocks <- get_best_match(cb = closest_blocks_normed, 
                                     min_year = max(t0_year - lookback, min_year), #min_year, 
                                     t0_year = t0_year, 
                                     matching_vars,
                                     screen_2020 = temp_screen)
    if (nrow(matched_blocks)==0) {
      if (VERBOSE) {print("no matched blocks, skipping...")}
      next
      }
    # add HD name
    matched_blocks$LABEL <- hd
    # add the matched blocks to our big data frame w/ 2 columns:
    mb <- dplyr::bind_rows(mb, matched_blocks)
  }

  # remove that first row that was just NA
  mb <- mb[2:nrow(mb),]
  
  return(mb)
}


add_back_year_data <- function(closest_blocks, geos_shp, inhd=NA, xwalk, back_year, t0_year) {
  #' Adds data from a past year for the matching process
  #' @param closest_blocks: (dataframe) the df w/ the HD blocks and their N
  #'                        closest blocks
  #' @param geos_shp: (sf object)
  #' @param inhd: T/F: whether the block is in an HD or not
  #' @param xwalk: (dataframe): the xwalk file we need to xwalk from the back 
  #'                year to the t0 year (aka the root year)
  #' @param back_year: (int) the back year we're crosswalking forward
  #' @param t0_year: (int) the root year
  #' 
  #' @returns rv: (dataframe) the back year data allocated forward to the 
  #'              root year, using the xwalk
  
  # create different variable stubs and suffixes to differentiate 
  # the data that's in vs out of HDs
  if (inhd) {
    stub = "_inhd"
    merge_var = "hd_id"
  } else {
    stub = "_nohd"
    merge_var = "nohd_id"
  }
  
  # if the back year just is the root year, no need to do any allocation
  # we can just tack on the data and return it:
  if (back_year==t0_year) {
    rv <- 
      dplyr::left_join(x = closest_blocks %>% mutate(join_key = !!sym(merge_var)), 
                       y = select(geos_shp, starts_with("geo_"), starts_with("n_")) %>%
                         sf::st_drop_geometry(.) %>%
                         mutate(pop_dens = n_tot / as.vector(geo_area_meters)) %>%
                         rename_with(~ paste0(., "_", back_year, stub)), 
                       by = c(join_key = paste0("geo_id_", back_year, stub))) %>%
      select(-join_key, -starts_with("n_other"))
    
    return(rv)
  }
  
  # if the back year is not the root year we'll need to xwalk the data and
  # allocate it forward. Start by joining the data to the crosswalk file:
  df_to_join <- 
    dplyr::left_join(x = xwalk,
                     y = select(geos_shp, starts_with("geo_"), starts_with("n_")) %>%
                       sf::st_drop_geometry(.),
                     by = c("geo_id_from"="geo_id")
    )
  
  
  # allocate the data from year tX to t0
  df_to_join <- 
    df_to_join %>%
    mutate(n_tot = n_tot * pct_from_in_to,
           n_white = n_white * pct_from_in_to,
           n_black = n_black * pct_from_in_to) %>%
    group_by(geo_id_to) %>%   
    summarise(n_tot = sum(n_tot),
              n_black = sum(n_black),
              n_white = sum(n_white)) %>%
    ungroup() 
  
  # merge the allocated data onto the closest blocks dataset and return it:
  rv <- 
    dplyr::left_join(x = closest_blocks %>% mutate(join_key = !!sym(merge_var)), 
                     y = df_to_join %>%
                       rename_with(~ paste0(., "_", back_year, stub)), 
                     by = c(join_key = paste0("geo_id_to_", back_year, stub))
    ) %>% select(-join_key, -starts_with("n_other"))
  
  return(rv)
}



divide <- function(x, y) {
  #' helper function to divide two values and return NA if there's an issue:
  tryCatch({
    result <- x / y
    if (length(result) == 0) {
      return(NA)
    }
    result
  },
  error = function(e) NA
  )
  
}


calc_normed_vals <- function(cb, year, t0_year) {
  #' @param cb (aka closest_blocks): the data frame of HD blocks and their closest 
  #'                        potential non-HD block matches, with the 
  #'                        demographic data added
  #' @param year: (int) the year we're calc'ing normed values for
  #' @param t0_year: (int) the root year
  #' 
  #' @returns rv: the closest blocks data w/ the normed variables added
  
  # get the maximum values to norm by; these will be the denominators
  max_n_tot  = max(c(cb[[paste0("n_tot_", year, "_inhd")]], 
                     cb[[paste0("n_tot_", year, "_nohd")]]), na.rm = T)
  
  # using the 97th percentile instead of the true max so outliers don't skew all
  # the normalized values towards zero
  max_pop_dens  = quantile(c(cb[[paste0("pop_dens_", year, "_inhd")]], 
                        cb[[paste0("pop_dens_", year, "_nohd")]]), na.rm = T, probs=.97)
  
  
  for (stub in c("inhd", "nohd")) {
    # number of total residents, normed by dividing by the block w/ the most residents in this year
    cb[[paste0("n_tot_", year, "_", stub, "_norm")]]   <- divide(cb[[paste0("n_tot_", year, "_", stub)]],  max_n_tot)
    # pct black residents
    cb[[paste0("n_black_", year, "_", stub, "_norm")]] <- divide(cb[[paste0("n_black_", year, "_", stub)]],  cb[[paste0("n_tot_", year, "_", stub)]])
    # pct white residents
    cb[[paste0("n_white_", year, "_", stub, "_norm")]] <- divide(cb[[paste0("n_white_", year, "_", stub)]],  cb[[paste0("n_tot_", year, "_", stub)]])
    
    # population density, normed by dividing by the most dense block in this year
    # if(year==t0_year) {
    pop_dens_val_norm <- divide(cb[[paste0("pop_dens_", year, "_", stub)]],  max_pop_dens)
    cb[[paste0("pop_dens_", year, "_", stub, "_norm")]] <- pop_dens_val_norm
    # }
  }
  
  return(cb)
}






get_best_match <- function(cb, min_year, t0_year, matching_vars, screen_2020=T) {
  #' @param cb: (data frame) the closest blocks w/ all the data added
  #' @param min_year: (int) the farthest year to go back to for matching
  #' @param t0_year: (int) the root year
  #' @param matching_vars: (vector of str) the variables to match on.
  #'                       could be "n_tot_", "pop_density_", "pct_black_",
  #'                       "pct_white_" or some combo of those
  #' @param screen_2020: (bool) whether to remove potential matches that don't
  #'                     have population in 2020
  #' 
  #' @returns cb: (dataframe) data frame subest to the "best" matches
  
  
  # remove any potential pairs for which we don't have population density,
  # or for which the root year population is less than 5; this will help avoid
  # situations where we match to blocks that just contain parks or office 
  # buildings
  cb <- cb %>% 
    filter(! 
             (
              is.na(!!sym(paste0("pop_dens_", t0_year, "_inhd_norm"))) | 
              is.na(!!sym(paste0("pop_dens_", t0_year, "_nohd_norm"))) |
              (!!sym(paste0("n_tot_", t0_year, "_inhd")) < 5) | 
              (!!sym(paste0("n_tot_", t0_year, "_nohd")) < 5)
             )
           )
  
  # replace all NaNs w/ NA
  cb <- cb %>% mutate_all(~ifelse(is.nan(.), NA, .))
  
  # optionally, omit potential matches that don't have any population in 2020
  if (screen_2020) {
    temp_screen <- 
      dplyr::left_join(nohd_xwalk_list[[paste0("2020_", t0_year)]],
                       sf::st_drop_geometry(geos_shp), 
                       by = c("geo_id_from"="geo_id")) %>%
      group_by(geo_id_to) %>%
      summarise(tot_pop = sum(n_tot, na.rm=T)) %>%
      filter(tot_pop  < 2) %>%
      pull(geo_id_to)
    
    cb <- cb[!(cb$nohd_id %in% temp_screen),]
  }
  
  # get the differences for each year and variable we'll match on:
  for (y in seq(min_year, t0_year, 10)) {
    for (var in matching_vars) {
      # get the absolute value of the difference
      cb[paste0("diff_", var, "_", y)] <-
        abs(
          cb[paste0(var, y, "_inhd_norm")] - 
          cb[paste0(var, y, "_nohd_norm")]
          )
      # create a flag for whether the difference value is actually defined
      # we don't want to count variables/years that have undefined 
      # differences bc there was just missing data
      cb[paste0("_diff_", var, "_", y, "present_flag")] <- 
        !is.na(cb[paste0("diff_", var, "_", y)])
    }
  }
  
  
  cb <- 
    cb %>%
    rowwise() %>%
    # get the number of values present in each row:
    mutate(n_present = sum(c_across(ends_with("present_flag")), na.rm = TRUE)) %>%
    # get the sum of the absolute differences; we want to minimize this:
    mutate(final_diff = sum(c_across(starts_with("diff_")), na.rm = TRUE) / n_present) %>%
    ungroup() %>%
    # label the best match
    group_by(hd_id) %>%
    # get the X most similar non-HD blocks to each HD
    mutate(match_rank = rank(final_diff, ties.method = "random")) %>%
    filter(match_rank <= 2) %>%
    # and choose the one that has the most years/variables of data:
    mutate(most_data_rank = rank(n_present, ties.method = "random")) %>%
    ungroup() %>%
    filter(most_data_rank == 1) %>%
    select(hd_id, nohd_id)
  
  return(cb)
}





create_ts <- function(n_nearest, min_year=1940, lookback=80, matching_vars=NA, screen_2020=T) {
  #' @param n_nearest: (int) the # of nearest blocks to consider as matches
  #' @param min_year: (int) the first year to use in the timeseries;
  #'                  defaults to 1940, the first year in which we have data
  #' @param lookback: (int) # years to go back for matching algo
  #' @param matching_vars: (vector of str) the variables to match on.
  #'                       could be "n_tot_", "pop_density_", "pct_black_",
  #'                       "pct_white_" or some combo of those
  #' @param screen_2020: (bool) whether to remove potential matches that don't
  #'                     have population in 2020
  #'                       
  #' @returns ts: (data frame) the time series (not yet cleaned)
  
  if(VERBOSE){print("getting matched blocks...")}
  
  # get all the potential non-HD matches for each HD:
  mb <- create_matches(n_nearest=n_nearest, 
                       min_year=min_year, 
                       lookback=lookback, 
                       matching_vars, 
                       screen_2020)
  
  if(VERBOSE) {print(paste(nrow(mb), "rows in mb"))}
  
  if(VERBOSE){print("matched blocks... now creating time series...")}
  
  # Now we need to create a timeseries for each pair of HD blocks and their 
  # non-HD matched blocks
  
  # loop through each row of the matching block data frame
  # for each HD and non-HD block, get the data for all other years for that block,
  # allocated to the "root year" or t0, the decennial year before the block was 
  # created.
  
  # here's the big time series data frame we're going to fill in:
  ts <- data.frame(hd_name=NA, year=NA, n_black=NA, n_white=NA, 
                   n_tot=NA, treat=NA,
                   root_hd_id=NA, root_nohd_id=NA, pair_id=NA)
  
  # loop through each row of the matches data frame and allocate the 
  # data back to the root year
  
  # later, the pair_id will identify each matched pair of HD and non-HD blocks:
  pair_id = 1
  for (i in 1:nrow(mb)) {
    if (VERBOSE & i %% 100 == 0) {cat(paste0(round(i/nrow(mb)*100,0),'% done...'))}
    root_year    <- as.numeric(stringr::str_sub(mb$hd_id[i], 1, 4))
    root_hd_id   <- mb$hd_id[i]
    root_nohd_id <- mb$nohd_id[i]
    hd_name   <- mb$LABEL[i]
    
    # don't match blocks that have basically no population in the root year
    root_year_pop <- geos_shp$n_tot[geos_shp$geo_id==root_hd_id]
    
    if (root_year_pop < 5) {next}
    
    # for this matched pair, go through each year and grab the data in each
    # year for this matched pair (both the HD and non-HD data)
    # we'll use this to cobble together our time series
    for (y in seq(min_year, 2020, 10)) {
      to_add <- data.frame(hd_name=NA, year=NA, n_black=NA, n_white=NA, 
                           n_tot=NA, treat=NA)
      
      if (y==root_year) {
        temp_hd_block    <- get_root_year_data(y, root_hd_id,   geos_shp, treat=1)
        temp_match_block <- get_root_year_data(y, root_nohd_id, geos_shp, treat=0)
        
        temp_to_add <- dplyr::bind_rows(temp_hd_block, temp_match_block)
        to_add <- dplyr::bind_rows(to_add, temp_to_add)
      } else {
        
        temp_inhd_xwalk <- inhd_xwalk_list[[paste0(y, "_", root_year)]]
        temp_nohd_xwalk <- nohd_xwalk_list[[paste0(y, "_", root_year)]]
        
        
        temp_hd_block    <- get_other_year_data(y, root_hd_id,   
                                                geos_shp, temp_inhd_xwalk, treat=1)
        temp_match_block <- get_other_year_data(y, root_nohd_id, 
                                                geos_shp, temp_nohd_xwalk, treat=0)
        
        if (nrow(temp_hd_block) > 1) {
          print("stop!")
        }
        if (nrow(temp_match_block) > 1) {
          print("again, stop!")
        }
        
        temp_to_add <- dplyr::bind_rows(temp_hd_block, temp_match_block)
        
        to_add <- dplyr::bind_rows(to_add, temp_to_add)
      }
      
      # add data common to this matched pair of blocks
      to_add$hd_name      <- hd_name
      to_add$root_hd_id   <- root_hd_id
      to_add$root_nohd_id <- root_nohd_id
      to_add$pair_id      <- pair_id
      to_add$year         <- y
      
      ts <- dplyr::bind_rows(ts, to_add[!is.na(to_add$year),])
      ts <- ts[!is.na(ts$geo_id),]
      
    }
    
    pair_id = pair_id + 1
  }
  invisible(gc())
  
  
  return(ts)
}


clean_ts <- function(ts, min_pop_tot=0, interpolate=T, remove_zero_blocks=F){
  #' This function just cleans the time series output by create_ts
  #' @param ts: (data frame) the rough time series data frame
  #' @param min_pop_tot: (int) the minimum population a block needs to have
  #'                     to actually be included in the analysis; defaults to 0
  #' @param interpolate: (bool) whether to use linear interpolation to 
  #'                     interpolate missing values. does not extrapolate if 
  #'                     values are missing on the end of the time series.
  #'                     defaults to TRUE.
  #' @param remove_zero_blocks: (bool) if TRUE, removes block pairs in which
  #'                            one or both of the blocks had zero population in
  #'                            a majority of the observed years.
  #'                                               
  #' @returns ts_clean: (data frame) the cleaned dataset
  
  # clean up data frame a bit
  ts <- ts %>% 
    select(-geo_area_meters) %>%
    # add the hd name and the designation date
    dplyr::left_join(select(hd_shp, LABEL, desig_date) %>% sf::st_drop_geometry(.), 
                     by=c("hd_name"="LABEL")) %>%
    # remove instance where the geo_id is not either the HD geo_id or the 
    # match geo_id (this shouldn't occur anyway)
    filter((geo_id == root_hd_id) | (geo_id == root_nohd_id)) %>%
    # make sure everything is summed up
    group_by(hd_name, pair_id, year, treat) %>%
    summarize(
           root_hd_id  = max(root_hd_id),
           root_nohd_id  = max(root_nohd_id),
           n_tot   = sum(n_tot, na.rm = F),
           n_black = sum(n_black, na.rm=F),
           n_white = sum(n_white, na.rm = F),
           desig_date = mean(desig_date)
           ) %>%

    ungroup() %>%
    # omit really small HDs (less than 10 acres) and Penn Ave, which
    # in most runs failed to match
    filter(!(hd_name %in% hds_to_omit)) 
  
  ts <- ts %>%
    # add basic variables we'll need for the diff in diff model
    mutate(desig_decade = floor(desig_date/10)*10) %>%
    mutate(post = ifelse(year >= desig_date, 1, 0)) %>%
    mutate(did_post = ifelse(treat==1, desig_decade + 10, 0)) %>%
    mutate(pct_white = n_white / n_tot) %>%
    mutate(pct_black = n_black / n_tot) %>%
    mutate(geo_id = ifelse(treat==1, root_hd_id, root_nohd_id)) %>%
    dplyr::left_join(geos_shp %>% select(geo_id, geo_area_meters) %>% sf::st_drop_geometry(.) %>% mutate(geo_area_meters = as.vector(geo_area_meters)),
                     by="geo_id") %>%
    mutate(pop_density = n_tot / geo_area_meters) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    group_by(hd_name, root_hd_id, treat) %>%
    mutate(did_unique_id = cur_group_id()) %>%
    ungroup() %>%
    # set population density to NA if we're missing other data:
    mutate(pop_density = ifelse(is.na(pct_white) & pop_density==0, NA, pop_density))
  
  # interpolate missing values, if that option is set to TRUE
  if (interpolate) {
    ts <- ts %>%
      group_by(hd_name, pair_id, treat) %>%
      arrange(year) %>%
      mutate(pct_white   = zoo::na.approx(pct_white, rule = 1, na.rm=FALSE),
             n_tot       = zoo::na.approx(n_tot, rule = 1, na.rm=FALSE),
             pop_density = zoo::na.approx(pop_density, rule = 1, na.rm=FALSE)) %>%
      ungroup() 
  }
  
  # note the # of missing observations in each pair
  ts <- ts %>%
    group_by(pair_id) %>%
    mutate(missing_pct_white = ifelse(is.na(pct_white), 1, 0),
           missing_pop_dense = ifelse(is.na(pop_density), 1, 0)) %>%
    ungroup() %>%
    # filter out small blocks, if the min_pop_tot value is set to a number 
    # greater than 0 (it's 0 by default)
    filter(n_tot >= min_pop_tot) %>%
    # final clean up
    mutate(pair_id = as.factor(pair_id),
           pct_white = ifelse(pct_white < 0, 0, pct_white),
           pct_black = ifelse(pct_black < 0, 0, pct_black)) %>%
    # convert from people per square meter to people per acre
    mutate(pop_density = pop_density * 1000000 / 247.105)
  
  if(remove_zero_blocks) {
    # remove block pairs where one or both of the pairs has zero population more than 
    # half of the data years. this is likely a little conservative, because it will
    # filter out commercial blocks that became residential later (which are
    # probably more common outside HDs)
    ts <-
      ts %>%
      ungroup() %>% group_by(pair_id, treat) %>%
      mutate(zeros_n_tot = ifelse(n_tot==0, 1, 0),
             n_zeros_n_tot = sum(zeros_n_tot),
             n_rows = n(),
             pct_zero = round(100*n_zeros_n_tot/n_rows, 0)) %>%
      ungroup() %>%
      group_by(pair_id) %>%
      mutate(max_pct_zeros = max(pct_zero)) %>% 
      ungroup() %>% 
      filter(max_pct_zeros < 50) %>%
      arrange(hd_name, pair_id, treat, year)
  }
    
  
  return(ts)
}




show_att_results <- function(dep_var, df, restrict_to_complete_ts_blocks=F, 
                             title="", show_resid_plots=F, beta_reg=F,
                             show_cohort_results=F,
                             geff = "dynamic", 
                             trim_year_threshold = -999,
                             cv="pair_id") {
  #' @param dep_var: (str) the dependent variable for the regressions
  #' @param df: (data frame) the time series data frame
  #' @param restrict_to_complete_ts_blocks: (boolean) whether to subset the data
  #'                                        to just the block pairs that have
  #'                                        many years of data without missing
  #'                                        observations
  #' @param title: (str) graph title
  #' 
  #' @returns None (this function just prints output)
  
  to_run <- df %>% 
    # filter out rows that are missing the outcome variable
    filter(!is.na(!!sym(dep_var))) %>% 
    mutate(row_num = row_number()) %>%
    group_by(pair_id, year) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    # create year variable that's relative to the t0 year
    mutate(rel_year = (year - desig_decade) / 10) %>%
    group_by(did_unique_id) %>%
    mutate(pre_n_tot = ifelse(post==0, mean(n_tot, na.rm=T), NA)) %>%
    mutate(pre_n_tot = max(pre_n_tot, na.rm=T)) %>%
    ungroup() %>%
    filter(rel_year >= trim_year_threshold)
  
  if (restrict_to_complete_ts_blocks) {
    to_run <-
      to_run %>%
      filter(count == 2)
  }
  
  
  cat(paste0("\n\n\n\n_____________________", title))
  
  # run difference in difference models
  
  # fixest package regression:
  if (beta_reg) {  
    cat("\n\n\n\n\t\t_____Betareg package results_____\n\n")
    betareg_formula <- formula(paste0(dep_var, " ~ post + treat + post*treat | as.factor(year) + as.factor(pair_id)"))
    mod <- betareg::betareg(formula = betareg_formula, 
                              data = to_run %>% 
                                mutate(pct_black = pmax( 0.0001, pmin( pct_black, 0.9999)),
                                       pct_white = pmax( 0.0001, pmin( pct_white, 0.9999))
                                       ), 
                              link = "log", 
                              control = betareg::betareg.control(maxit = 10000))
    
    print(summary(mod, phi = FALSE))
    if (show_resid_plots) {plot(mod)}
    
    cat("\n\n\t\t_____DiD package results_____\n\n\n")
    attgt <- did::att_gt(yname = dep_var,
                         gname = "did_post",
                         idname = "did_unique_id",
                         tname = "year",
                         xformla = ~1,
                         data =  to_run,
                         clustervars = cv,
                         weightsname="n_tot",
                         allow_unbalanced_panel = T
    )
    # print(summary(attgt))
    group_effects <- aggte(attgt, type = geff, na.rm=T)
    print(summary(group_effects))
    print(ggdid(group_effects, title=title, theming=F) + ggdark::dark_theme_gray())
    
  } else {
    # did package regression:
    cat("\n\n\t\t_____DiD package results_____\n\n\n")
    attgt <- did::att_gt(yname = dep_var,
                         gname = "did_post",
                         idname = "did_unique_id",
                         tname = "year",
                         xformla = ~1,
                         data =  to_run,
                         clustervars = cv,
                         weightsname="n_tot",
                         allow_unbalanced_panel = T
    )
    print(summary(attgt))
    group_effects <- aggte(attgt, type = geff, na.rm=T)
    print(summary(group_effects))
    print(ggdid(group_effects, title=title, theming=F) + ggdark::dark_theme_gray())
    
    if (show_cohort_results) {
      group_effects <- aggte(attgt, type = "group", na.rm=T)
      print(summary(group_effects))
      print(ggdid(group_effects))
    }
    
  }
  
}



show_buffer_att_results <- function(hds_filter_out=c(""), show_resid_plots=F,
                                    dep_var, buffer_size, beta_reg=F,
                                    title="") {
  #' @param hds_filter_out: (vector) HDs to remove
  #' @param show_resid_plots: (bool) whether to show residual plots
  #' @param dep_var: (str) the dependent variable we want to regress on
  #' @param buffer_size: (int) the buffer size in meters
  #' @param beta_reg: (bool) whether to run a beta regression
  #' @param title: (str) the title to give our plot
  #' 
  #' @returns None: just prints things; doesn't return anything

  buffer <- sf::st_buffer(x = filter(hd_shp, !(LABEL %in% hds_filter_out)), dist = buffer_size)
  
  
  nohd_blocks <- sf::st_intersection(x = geos_shp[geos_shp$geo_id %in% geos_outside_hds,], buffer) %>% mutate(treat=0) %>% sf::st_drop_geometry(.)
  hd_blocks <- dplyr::left_join(geos_in_hds, select(geos_shp, -year), by="geo_id") %>% mutate(treat=1)
  
  buffer_ts <- dplyr::bind_rows(hd_blocks, nohd_blocks) %>%
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
           pct_black, pct_white) %>%
    filter(!(LABEL %in% hds_filter_out)) %>%
    mutate(pop_density = pop_density * 1000000 / 247.105) 
  
  if ( length(unique(buffer_ts$LABEL))==1 ) {
    cvars=NULL
    single_hd=T
  } else {
      cvars="LABEL"
      single_hd = F
      }
  
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
  # print(summary(attgt))
  group_effects <- aggte(attgt, type = "dynamic", na.rm=T)
  print(summary(group_effects))
  print(ggdid(group_effects, title=title, theming=F) + 
          ggdark::dark_theme_gray())
  
  if (beta_reg) {
    betareg_formula <- formula(paste0(dep_var, " ~ post + treat + post*treat | as.factor(year) + as.factor(LABEL)"))
    mod <- betareg::betareg(formula = betareg_formula, 
                            data = ts %>% 
                              mutate(pct_white = pmax( 0.0001, pmin( pct_white, 0.9999)),
                                     pct_black = pmax( 0.0001, pmin( pct_black, 0.9999))
                                     ), 
                            link = "log", 
                            control = betareg::betareg.control(maxit = 10000))
    
    print(summary(mod), phi=F)
    if (show_resid_plots) {plot(mod)}
    
  } 
}



get_differences_and_outliers <- function(ts, lower=2.5, upper=97.5) {
  #' This function takes our time series and shows the differences in our
  #' variables of interest (pop density, pct black, pct white) before the
  #' treatment is applied. It also returns a list with two vectors of pair ids,
  #' so we can run a robustness check where we omit those pair ids that are
  #' outliers from the diff-in-diff model. The function treats < p2.5 or >p97.5
  #' as an outlier.
  #' 
  #' @param ts: (data frame) the time series data set
  #' @param lower: (numeric) the lower percentile cut off to use
  #' @param upper: (numeric) the upper percentile cut off to use
  #' @returns: (list) list of pair_ids, for pairs that are super different 
  #'           between treatment and control before the treatment is applied
  
  # filter to just the pre treatment data
  temp_df <-
    ts %>% 
    filter(post==0) %>% 
    group_by(year, hd_name, pair_id, treat) %>% 
    summarize(`mean population density (people/acre)` =round( weighted.mean(pop_density, w = n_tot), 0),
              `percent black` = round(weighted.mean(pct_black, w=n_tot, na.rm=T)*100, 0),
              `percent white` = round(weighted.mean(pct_white, w=n_tot, na.rm=T)*100, 0)
    ) 
  
  # pivot the data to wide so we can easily get the treatment vs control
  # group differences
  wide_data <-
    temp_df %>%
    tidyr::pivot_wider(data = ., 
                       id_cols = c(year, hd_name, pair_id), 
                       names_from = treat, 
                       values_from=c(`mean population density (people/acre)`, `percent black`, `percent white`)) %>%
    mutate(pop_dens_diff  = `mean population density (people/acre)_1` - `mean population density (people/acre)_0`,
           pct_black_diff = `percent black_1` - `percent black_0`,
           pct_white_diff = `percent white_1` - `percent white_0`) %>%
    ungroup()
  
  p <-
    wide_data %>%
    summarise(across(c(pop_dens_diff, pct_black_diff, pct_white_diff), 
                     ~quantile(.x, probs =  seq(0, 1, by = 0.025), na.rm = TRUE))) %>% 
    mutate(percentile = seq(0, 100, 2.5)) %>%
    select(percentile, pop_dens_diff, pct_black_diff, pct_white_diff)
  
  # show the pre-treatment differences in a table
  print(p %>% rename(`pop. density diff percentile values` = pop_dens_diff,
                     `black percentage point diff percentile values` = pct_black_diff,
                     `white percentage point diff percentile values` = pct_white_diff) %>%
          knitr::kable()
  )
  
  print(
    ggplot(data = wide_data, aes(x=pop_dens_diff)) +
      geom_histogram() + 
      scale_x_continuous(limits = c(-50, 50)) +
      ggtitle("Distibution of differences between treat & control blocks (pop. density)") +
      xlab("Difference in ppl/acre btw treat and control pairs") +
      ggdark::dark_theme_gray()
    )
  
  print(
    ggplot(data = wide_data, aes(x=pct_white_diff)) +
      geom_histogram() + 
      scale_x_continuous(limits = c(-50, 50)) +
      ggtitle("Distibution of differences between treat & control blocks (% white residents)") +
      xlab("Difference in percentage points btw treat and control pairs") +
      ggdark::dark_theme_gray()
  )
  
  # get the pairs of IDs that are outliers
  pop_dens_outlier_pairs <-
    temp_df %>%
    ungroup() %>%
    group_by(year, hd_name, pair_id) %>%
    summarise(across(c(`mean population density (people/acre)`, `percent black`, `percent white`), diff, .names = '{col} difference')) %>%
    filter(`mean population density (people/acre) difference` < p$pop_dens_diff[p$percentile==lower] |
             `mean population density (people/acre) difference` > p$pop_dens_diff[p$percentile==upper]
    ) %>% 
    pull(pair_id)
  
  pct_black_outlier_pairs <-
    temp_df %>%
    ungroup() %>%
    group_by(year, hd_name, pair_id) %>%
    summarise(across(c(`mean population density (people/acre)`, `percent black`, `percent white`), diff, .names = '{col} difference')) %>%
    filter(`percent black difference` < p$pct_black_diff[p$percentile==lower] |
             `percent black difference` > p$pct_black_diff[p$percentile==upper]
    ) %>% 
    pull(pair_id)
  
  pct_white_outlier_pairs <-
    temp_df %>%
    ungroup() %>%
    group_by(year, hd_name, pair_id) %>%
    summarise(across(c(`mean population density (people/acre)`, `percent black`, `percent white`), diff, .names = '{col} difference')) %>%
    filter(`percent white difference` < p$pct_white_diff[p$percentile==lower] |
             `percent white difference` > p$pct_white_diff[p$percentile==upper]
    ) %>% 
    pull(pair_id)
  
  return(
    list("pop_dens_outlier_pairs"=pop_dens_outlier_pairs,
         "pct_white_outlier_pairs"=pct_white_outlier_pairs)
  )
}





get_pval <- function(attgt) {
  # gets the p-value for the parallel trends assumption test in a did regression
  pval_txt <- paste(invisible(capture.output(summary(attgt))), collapse = "\n")
  regex_pattern <- paste0("(?<=", "P-value for pre-test of parallel trends assumption: ", ").*?(?=\n)")
  pval_txt <- as.numeric(trimws(stringr::str_extract(pval_txt, regex_pattern)))
  
  return(pval_txt)
}




do_buffer_analysis <- function(buffer_size, hds_to_omit, cvars, 
                               dep_var, 
                               return_shps=F,
                               max_years_back=100,
                               wt="n_tot",
                               unzoned_threshold=1.1,
                               just_return_ts=F,
                               bp="varying") {
  #' This function runs the buffer analysis.
  #' @param buffer_size (int) the size of the buffer to draw in meters
  #' @param hds_to_omit (vector of str) HDs to omit from analysis
  #' @param cvars (str) the level at which to cluster the errors
  #' @param dep_var (str) the dependent variable
  #' @param return_shps (bool) whether or not to return the sf objects in the analysis
  #' @param max_years_back (int) the number of pre-treatment years to include in 
  #'                             the analysis. Set to a large number to make this
  #'                             basically do nothing.
  #' @param wt (str) the weighting variable to use
  #' @param unzoned_threshold (double) whether to filter out blocks with 
  #'                                   X% or more unzoned land. Set this above 1
  #'                                   to avoid filtering on this entirely.
  #' @param just_return_ts (bool) just return the timeseries data frame without
  #'                              doing the main analysis
  #' @param bp (str) the base period approach to use in the regression;
  #'                 can be "varying" or "universal". See did package for more.
  #' @returns (list) list of objects that are the result of the analysis
  
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
    # fix a few instances (6) where n_white < 0
    mutate(n_white = ifelse(n_white < 0, 0, n_white)) %>% 
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
    filter(year > (desig_decade - max_years_back)) %>%
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
  
  if (just_return_ts) {
    return(buffer_ts)
  }
  # create data sets to plot changes in density & demographics before / after HD creation
  to_plot <-
    buffer_ts %>%
    group_by(LABEL, treat, post) %>%
    summarize(outcome_var = mean(.data[[dep_var]], na.rm=T),
              hd_area_acres = max(hd_area_acres, na.rm=T)) %>%
    mutate(change = outcome_var - lag(outcome_var), 1) %>%
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
                       weightsname=wt,
                       allow_unbalanced_panel = T,
                       base_period = bp,
                       panel = F
  )
  
  if (return_shps) {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts,
                "outer_buffer"=outer_buffer, 'inner_buffer'=inner_buffer,
                'nohd_blocks'=nohd_blocks, 'hd_blocks'=hd_blocks
                )
           )
  } else {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts))
  }
}





do_buffer_analysis_w_max_extent <- function(buffer_size, hds_to_omit, cvars, 
                               dep_var, 
                               return_shps=F,
                               max_years_back,
                               only_extend_outer_buffer=F,
                               subset_to_smaller_blocks=F,
                               wt="n_tot",
                               unzoned_threshold=1.1,
                               just_return_ts=F,
                               bp="varying") {
  #' This function is similar to the one above, but allows the user to set
  #' different parameters as robustness checks. For example, the user can,
  #' 1. Drop X number of pre-treatment years
  #' 2. Extend the buffer in all years to cover the maximum area that the 
  #'    buffer covered in any given year
  #' 3. only do (2) for the control blocks
  #' 4. remove blocks over a certain size from the analysis
  #' All of these tweaks are to try and see if the results hold after trying to
  #' account for the fact that the areas involved change slightly over time,
  #' as the size of the blocks change slightly over time.
  #' @param buffer_size (int) the size of the buffer to draw in meters
  #' @param hds_to_omit (vector of str) HDs to omit from analysis
  #' @param cvars (str) the level at which to cluster the errors
  #' @param dep_var (str) the dependent variable
  #' @param return_shps (bool) whether or not to return the sf objects in the analysis
  #' @param max_years_back (int) the number of pre-treatment years to include in 
  #'                             the analysis
  #' @param only_extend_outer_buffer (bool) whether to only extend the buffer for
  #'                                        the control blocks.
  #' @param subset_to_smaller_blocks (bool) whether to move larger blocks from the
  #'                                        analysis entirely
  #' @param wt (str) the weighting variable to use
  #' @param unzoned_threshold (double) whether to filter out blocks with 
  #'                                   X% or more unzoned land. Set this above 1
  #'                                   to avoid filtering on this entirely.
  #' @param just_return_ts (bool) just return the timeseries data frame without
  #'                              doing the main analysis
  #' @param bp (str) the base period approach to use in the regression;
  #'                 can be "varying" or "universal". See did package for more.
  #' @returns (list) list of objects that are the result of the analysis
  
  
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
  
  # loop through each HD, get the max extent of the blocks touched by a 
  # buffer in any year, then get all the blocks in later years in that extent:
  i = 1
  for (hd in hd_subset$LABEL) {
    
    hd_desig_decade <- hd_shp %>% filter(LABEL == hd) %>% pull(desig_decade)
    
    if (VERBOSE) {
      cat(paste("\n\nHD:", hd, "desig decade", hd_desig_decade))
    }
    
    max_extent_outer_shp <- 
      sf::st_union(geos_shp %>% 
                     filter(geo_id %in% nohd_blocks$geo_id[nohd_blocks$LABEL==hd]) %>%
                     filter(year > (hd_desig_decade - max_years_back))
                   )
    max_extent_inner_shp <- 
      sf::st_union(geos_shp %>% 
                     filter(geo_id %in% hd_blocks$geo_id[hd_blocks$LABEL==hd]) %>%
                     filter(year > (hd_desig_decade - max_years_back))
                   )
    
    nohd_blocks_temp <- 
      get_geos_in_shp(shp = geos_shp[geos_shp$geo_id %in% geos_outside_hds,], 
                      min_pct = .2, 
                      parent_shp = max_extent_outer_shp, 
                      geo_id = "geo_id", 
                      parent_id = "year"
      ) %>%
      filter(year > (hd_desig_decade - max_years_back)) %>%
      mutate(LABEL = hd, desig_decade = hd_desig_decade)
    
    hd_blocks_temp   <- 
      get_geos_in_shp(shp = geos_shp, 
                      min_pct = .2, 
                      parent_shp = max_extent_inner_shp, 
                      geo_id = "geo_id", 
                      parent_id = "year"
      ) %>%
      filter(year > (hd_desig_decade - max_years_back)) %>%
      mutate(LABEL = hd, , desig_decade = hd_desig_decade)
    
    # build the dataframe:
    if (i==1) {
      hd_blocks_list <- hd_blocks_temp
      nohd_blocks_list <- nohd_blocks_temp
    } else {
      hd_blocks_list <- dplyr::bind_rows(hd_blocks_temp, hd_blocks_list)
      nohd_blocks_list <- dplyr::bind_rows(nohd_blocks_temp, nohd_blocks_list)
    }
    i = i + 1
  }
    
    nohd_blocks_shp <- 
      geos_shp %>% 
      filter(geo_id %in% nohd_blocks_list$geo_id) %>%
      filter(!(geo_id %in% geos_in_hds)) %>%
      filter(n_tot > 10) %>%
      dplyr::left_join(
        y = nohd_blocks_list %>% select(geo_id, LABEL, desig_decade),
        by = "geo_id"
          ) %>%
      mutate(treat=0) 
    
    hd_blocks_shp <-
      geos_shp %>% 
      filter(geo_id %in% hd_blocks_list$geo_id) %>%
      filter(!(geo_id %in% geos_outside_hds)) %>%
      filter(n_tot > 10) %>%
      dplyr::left_join(y = geos_in_hds %>% select(-year), by="geo_id") %>%
      mutate(treat=1)
  
    if (only_extend_outer_buffer) {
      hd_blocks_shp <- 
        geos_shp[geos_shp$geo_id %in% hd_blocks$geo_id,] %>%
        dplyr::left_join(select(geos_in_hds, -year), by="geo_id") %>%
        mutate(treat=1)
    }
    
  # create the timeseries by stacking the in-HD blocks and the outside-HD blocks
  buffer_ts <- 
    dplyr::bind_rows(hd_blocks_shp, nohd_blocks_shp) %>%
    sf::st_drop_geometry(.) %>%
    # fix a few instances (6) where n_white < 0
    mutate(n_white = ifelse(n_white < 0, 0, n_white)) %>% 
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
                     by = "LABEL") %>%
    filter(year > (desig_decade - max_years_back))
  
  if(subset_to_smaller_blocks) {
    buffer_ts <- filter(buffer_ts, as.vector(geo_area_meters) < 50000)
  }
  
  # remove blocks that are over X% unzoned land
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
  
  if (just_return_ts) {
    return(buffer_ts)
  }
  # create data sets to plot changes in density & demographics before / after HD creation
  to_plot <-
    buffer_ts %>%
    group_by(LABEL, treat, post) %>%
    summarize(outcome_var = mean(.data[[dep_var]], na.rm=T),
              hd_area_acres = max(hd_area_acres, na.rm=T)) %>%
    mutate(change = outcome_var - lag(outcome_var), 1) %>%
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
                       weightsname=wt,
                       allow_unbalanced_panel = T,
                       base_period = bp,
                       panel = F
  )
  
  if (return_shps) {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts,
                "outer_buffer"=outer_buffer, 'inner_buffer'=inner_buffer,
                "max_extent_outer_shp"=max_extent_outer_shp,
                "max_extent_inner_shp"=max_extent_inner_shp,
                'nohd_blocks'=nohd_blocks, 'hd_blocks'=hd_blocks
    )
    )
  } else {
    return(list("p1"=p1, "p2"=p2, "attgt"=attgt, "to_plot_2"=to_plot_2,
                "buffer_ts"=buffer_ts))
  }
}







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
                                alpha_level=.05,
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
                                  l_vec          = baseVec1,
                                  alpha=alpha_level)
  
  if (type=="relative_magnitude") {
    robust_ci <- createSensitivityResults_relativeMagnitudes(betahat        = beta,
                                                             sigma          = V,
                                                             numPrePeriods  = npre,
                                                             numPostPeriods = npost,
                                                             l_vec          = baseVec1,
                                                             gridPoints     = gridPoints,
                                                             alpha=alpha_level)
    
  } else if (type == "smoothness") {
    robust_ci <- createSensitivityResults(betahat        = beta,
                                          sigma          = V,
                                          numPrePeriods  = npre,
                                          numPostPeriods = npost,
                                          l_vec          = baseVec1,
                                          alpha=alpha_level)
  }
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}











# (Had Claude.ai write some of the function below)

#' Create Regression Diagnostic Plots for Simple DID with Repeated Cross-Sections
#'
#' @param data A dataframe containing your repeated cross-sectional data
#' @param outcome_var Name of the outcome variable (string)
#' @param time_var Name of the time period variable (string)
#' @param id_var Name of the neighborhood/cluster ID variable (string)
#' @param treat_var Name of the treatment indicator variable (string), should be 1 if treated
#' @param covariates Optional vector of covariate names to include (default NULL)
#' 
#' @return A list of ggplot objects with diagnostic plots
#' @export
#'
#' @examples
#' diagnostics <- did_diagnostics(
#'   data = my_data,
#'   outcome_var = "income",
#'   time_var = "year",
#'   id_var = "neighborhood_id",
#'   treat_var = "treated"
#' )

main_did_diagnostics <- function(data, outcome_var, time_var, id_var, 
                                 treat_var, post_var, covariates = NULL,
                                 wt = NULL) {
  
  # Create formula for regression
  formula_str <- paste0(outcome_var, " ~ ", 
                        treat_var, " + ",
                        post_var, " + ",
                        treat_var, "*", post_var, " + ",
                        "factor(", time_var, ") + factor(", id_var, ")"
                        )
  
  # Fit the model
  model <- lm(as.formula(formula_str), data = data, weights = wt)
  
  # Parallel Trends Visual Check (Pre-treatment periods)
  # Calculate mean outcomes by treatment group and time
  trends_data <- data %>%
    group_by(!!sym(time_var), !!sym(treat_var)) %>%
    summarise(mean_outcome = mean(get(outcome_var), na.rm = TRUE),
              .groups = "drop")
  
  p7 <- ggplot(trends_data, aes(x = get(time_var), y = mean_outcome, 
                                color = factor(get(treat_var)), 
                                group = factor(get(treat_var)))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Mean Outcome Over Time by Treatment Group",
         subtitle = "Visual check for parallel trends assumption",
         x = "Time Period",
         y = paste("Mean", outcome_var),
         color = "Treatment Group") +
    theme_minimal() +
    scale_color_manual(values = c("0" = "blue", "1" = "red"),
                       labels = c("0" = "Control", "1" = "Treated"))
  
  # Display all plots
  par(mfrow = c(2, 2))
  print(plot(model))
  par(mfrow = c(1, 1))
  print(p7)
  
  # Print summary statistics
  cat("\n=== Model Summary ===\n")
  print(summary(model))
  
}

# Example usage:
# diagnostics <- did_diagnostics(
#   data = my_census_data,
#   outcome_var = "median_income",
#   time_var = "year",
#   id_var = "neighborhood_id",
#   treat_var = "treated_indicator",
#   covariates = c("population", "pct_white")
# )













# (Had Claude.ai write some of the function below)

library(lmtest)  # for Breusch-Pagan test
library(lfe)     # for high-dimensional fixed effects (optional but recommended)

#' Outcome Regression Diagnostics for Difference-in-Differences
#' 
#' @param data A data frame containing the panel data
#' @param yname Character string for outcome variable name
#' @param tname Character string for time variable name
#' @param idname Character string for unit ID variable name (e.g., Census block)
#' @param gname Character string for treatment timing variable name
#' @param xformla Formula for covariates (e.g., ~lpop + x2)
#' @param time_fe Logical. Include time fixed effects? (Default: TRUE)
#' @param unit_fe Logical. Include unit fixed effects? (Default: FALSE)
#' @param parent_geo Character string for parent geography variable (e.g., county, MSA). 
#'                   If provided, adds parent geography fixed effects.
#' @param cluster_var Character string for clustering standard errors (typically idname or parent_geo)
#' @param control_group Either "nevertreated" or "notyettreated"
#' @param use_felm Logical. Use felm() for high-dimensional FE? Requires 'lfe' package. (Default: FALSE)
#' @param return_plots Logical. If TRUE, returns list of plots. If FALSE, displays them
#' @param save_plots Logical. If TRUE, saves plots to files
#' @param output_dir Directory to save plots (if save_plots = TRUE)
#' 
#' @return A list containing:
#'   - plots: List of ggplot objects
#'   - residual_data: Data frame with residuals and diagnostics
#'   - model: The fitted outcome regression model
#'   - tests: List of statistical test results
#'   - summary: Summary statistics and diagnostics
#' 

did_outcome_diagnostics <- function(data,
                                    yname,
                                    tname,
                                    idname,
                                    gname,
                                    xformla = NULL,
                                    time_fe = TRUE,
                                    unit_fe = FALSE,
                                    parent_geo = NULL,
                                    cluster_var = NULL,
                                    control_group = "nevertreated",
                                    use_felm = FALSE,
                                    return_plots = TRUE,
                                    save_plots = FALSE,
                                    output_dir = "./did_diagnostics/") {
  
  # Create output directory if saving plots
  if(save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("=======================================================\n")
  cat("DIFFERENCE-IN-DIFFERENCES OUTCOME REGRESSION DIAGNOSTICS\n")
  cat("=======================================================\n\n")
  
  # Check if lfe is available if use_felm = TRUE
  if(use_felm && !requireNamespace("lfe", quietly = TRUE)) {
    warning("Package 'lfe' not installed. Falling back to lm(). Install with: install.packages('lfe')")
    use_felm <- FALSE
  }
  
  # ============================================================================
  # STEP 1: EXTRACT CONTROL GROUP DATA
  # ============================================================================
  
  cat("STEP 1: Extracting control group data...\n")
  
  if(control_group == "nevertreated") {
    control_data <- data %>%
      filter(get(gname) == 0)
  } else {
    # For notyettreated, include all observations before treatment
    control_data <- data %>%
      filter(get(gname) == 0 | get(tname) < get(gname))
  }
  
  n_units <- length(unique(control_data[[idname]]))
  n_obs <- nrow(control_data)
  time_range <- range(control_data[[tname]])
  
  cat(sprintf("  Control group size: %d observations from %d units\n", n_obs, n_units))
  cat(sprintf("  Time period: %d to %d\n", time_range[1], time_range[2]))
  
  if(!is.null(parent_geo)) {
    n_parent <- length(unique(control_data[[parent_geo]]))
    cat(sprintf("  Parent geographies (%s): %d\n", parent_geo, n_parent))
  }
  cat("\n")
  
  # ============================================================================
  # STEP 2: BUILD AND FIT OUTCOME REGRESSION MODEL
  # ============================================================================
  
  cat("STEP 2: Fitting outcome regression model...\n")
  cat("  Fixed effects included:\n")
  
  # Build formula components
  fe_terms <- c()
  
  # Covariates
  if(!is.null(xformla)) {
    covariates <- all.vars(xformla)
    covariate_str <- paste(covariates, collapse = " + ")
    cat(sprintf("    - Covariates: %s\n", covariate_str))
  } else {
    covariates <- NULL
    covariate_str <- "1"
  }
  
  # Time fixed effects
  if(time_fe) {
    fe_terms <- c(fe_terms, paste0("factor(", tname, ")"))
    cat(sprintf("    - Time FE: Yes (factor(%s))\n", tname))
  }
  
  # Unit fixed effects
  if(unit_fe) {
    fe_terms <- c(fe_terms, paste0("factor(", idname, ")"))
    cat(sprintf("    - Unit FE: Yes (factor(%s))\n", idname))
  }
  
  # Parent geography fixed effects
  if(!is.null(parent_geo)) {
    fe_terms <- c(fe_terms, paste0("factor(", parent_geo, ")"))
    cat(sprintf("    - Parent geography FE: Yes (factor(%s))\n", parent_geo))
  }
  
  # Choose estimation method
  if(use_felm && (unit_fe || !is.null(parent_geo))) {
    cat("  Using felm() for high-dimensional fixed effects\n")
    
    # Build felm formula: y ~ x | fixed effects | 0 | cluster
    fe_formula_part <- paste(fe_terms, collapse = " + ")
    cluster_part <- ifelse(!is.null(cluster_var), cluster_var, "0")
    
    model_formula <- as.formula(paste0(yname, " ~ ", covariate_str, 
                                       " | ", fe_formula_part, 
                                       " | 0 | ", cluster_part))
    
    cat("  Formula:", deparse(model_formula), "\n")
    outcome_model <- lfe::felm(model_formula, data = control_data)
    
  } else {
    # Use standard lm
    cat("  Using lm() for estimation\n")
    
    all_terms <- c(covariate_str, fe_terms)
    model_formula <- as.formula(paste(yname, "~", paste(all_terms, collapse = " + ")))
    
    cat("  Formula:", deparse(model_formula), "\n")
    outcome_model <- lm(model_formula, data = control_data)
  }
  
  # Get model fit statistics
  if(use_felm) {
    r_squared <- summary(outcome_model)$r.squared
    adj_r_squared <- summary(outcome_model)$adj.r.squared
  } else {
    r_squared <- summary(outcome_model)$r.squared
    adj_r_squared <- summary(outcome_model)$adj.r.squared
  }
  
  cat(sprintf("  R-squared: %.3f\n", r_squared))
  cat(sprintf("  Adjusted R-squared: %.3f\n\n", adj_r_squared))
  
  # ============================================================================
  # STEP 3: EXTRACT RESIDUALS AND DIAGNOSTICS
  # ============================================================================
  
  cat("STEP 3: Computing residual diagnostics...\n")
  
  residual_data <- data.frame(
    residual = residuals(outcome_model),
    fitted = fitted(outcome_model),
    actual = control_data[[yname]],
    time = control_data[[tname]],
    id = control_data[[idname]]
  )
  
  # Add parent geography if present
  if(!is.null(parent_geo)) {
    residual_data$parent_geo <- control_data[[parent_geo]]
  }
  
  # Add covariates if present
  if(!is.null(xformla)) {
    for(covar in all.vars(xformla)) {
      residual_data[[covar]] <- control_data[[covar]]
    }
  }
  
  # Diagnostic statistics (may not be available for felm)
  if(use_felm) {
    # felm doesn't provide all diagnostics, compute manually
    residual_data$standardized <- residual_data$residual / sd(residual_data$residual)
    residual_data$sqrt_abs_resid <- sqrt(abs(residual_data$residual))
    residual_data$cooks_d <- NA  # Not available for felm
    residual_data$leverage <- NA
    residual_data$studentized <- NA
  } else {
    residual_data$standardized <- rstandard(outcome_model)
    residual_data$studentized <- rstudent(outcome_model)
    residual_data$cooks_d <- cooks.distance(outcome_model)
    residual_data$leverage <- hatvalues(outcome_model)
    residual_data$sqrt_abs_resid <- sqrt(abs(residual_data$residual))
  }
  
  # Summary statistics
  resid_mean <- mean(residual_data$residual)
  resid_sd <- sd(residual_data$residual)
  n_outliers <- sum(abs(residual_data$standardized) > 2.5, na.rm = TRUE)
  max_cooks <- ifelse(use_felm, NA, max(residual_data$cooks_d, na.rm = TRUE))
  
  cat(sprintf("  Mean residual: %.4f (should be ~0)\n", resid_mean))
  cat(sprintf("  SD of residuals: %.3f\n", resid_sd))
  cat(sprintf("  Outliers (|std. resid| > 2.5): %d (%.1f%%)\n", 
              n_outliers, 100*n_outliers/n_obs))
  if(!use_felm) {
    cat(sprintf("  Max Cook's distance: %.3f\n", max_cooks))
  }
  cat("\n")
  
  # ============================================================================
  # STEP 4: STATISTICAL TESTS
  # ============================================================================
  
  cat("STEP 4: Running statistical tests...\n")
  
  tests <- list()
  
  # Shapiro-Wilk test for normality (on sample if n > 5000)
  if(n_obs <= 5000) {
    tests$shapiro <- shapiro.test(residual_data$residual)
    cat(sprintf("  Shapiro-Wilk normality test: p = %.4f ", tests$shapiro$p.value))
    if(tests$shapiro$p.value < 0.05) {
      cat("(reject normality)\n")
    } else {
      cat("(do not reject normality)\n")
    }
  } else {
    sample_resid <- sample(residual_data$residual, 5000)
    tests$shapiro <- shapiro.test(sample_resid)
    cat(sprintf("  Shapiro-Wilk test (n=5000 sample): p = %.4f ", tests$shapiro$p.value))
    if(tests$shapiro$p.value < 0.05) {
      cat("(reject normality)\n")
    } else {
      cat("(do not reject normality)\n")
    }
  }
  
  # Breusch-Pagan test (only for lm, not felm)
  if(!use_felm) {
    tests$bp <- bptest(outcome_model)
    cat(sprintf("  Breusch-Pagan heteroscedasticity test: p = %.4f ", tests$bp$p.value))
    if(tests$bp$p.value < 0.05) {
      cat("(reject homoscedasticity)\n")
    } else {
      cat("(do not reject homoscedasticity)\n")
    }
  } else {
    cat("  Breusch-Pagan test: Not available with felm\n")
    tests$bp <- NULL
  }
  
  # Durbin-Watson test (only for lm)
  if(!use_felm) {
    tests$dw <- dwtest(outcome_model)
    cat(sprintf("  Durbin-Watson autocorrelation test: DW = %.3f, p = %.4f\n", 
                tests$dw$statistic, tests$dw$p.value))
  } else {
    cat("  Durbin-Watson test: Not available with felm\n")
    tests$dw <- NULL
  }
  
  cat("\n")
  
  # ============================================================================
  # STEP 5: CREATE DIAGNOSTIC PLOTS
  # ============================================================================
  
  cat("STEP 5: Creating diagnostic plots...\n\n")
  
  plots <- list()
  
  # Plot 1: Residuals vs Fitted
  plots$resid_fitted <- ggplot(residual_data, aes(x = fitted, y = residual)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
    geom_smooth(se = TRUE, color = "darkred", method = "loess") +
    geom_hline(yintercept = c(-2, 2) * resid_sd, 
               linetype = "dotted", color = "orange") +
    theme_minimal() +
    labs(title = "Residuals vs Fitted Values",
         subtitle = "Should show random scatter around 0",
         x = "Fitted Values", y = "Residuals")
  
  # Plot 2: Q-Q Plot
  plots$qq <- ggplot(residual_data, aes(sample = residual)) +
    stat_qq(alpha = 0.5, color = "steelblue") +
    stat_qq_line(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Q-Q Plot: Normality Check",
         subtitle = paste("Shapiro-Wilk p =", round(tests$shapiro$p.value, 4)),
         x = "Theoretical Quantiles", y = "Sample Quantiles")
  
  # Plot 3: Scale-Location
  bp_subtitle <- ifelse(!is.null(tests$bp), 
                        paste("Breusch-Pagan p =", round(tests$bp$p.value, 4)),
                        "Visual check for heteroscedasticity")
  
  plots$scale_location <- ggplot(residual_data, aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(se = TRUE, color = "darkred", method = "loess", size = 1) +
    theme_minimal() +
    labs(title = "Scale-Location Plot",
         subtitle = bp_subtitle,
         x = "Fitted Values", y = "√|Standardized Residuals|")
  
  # Plot 4: Residuals over Time
  time_summary <- residual_data %>%
    group_by(time) %>%
    summarise(
      mean_resid = mean(residual),
      se_resid = sd(residual) / sqrt(n()),
      .groups = "drop"
    )
  
  plots$time <- ggplot(time_summary, aes(x = time, y = mean_resid)) +
    geom_line(size = 1, color = "darkblue") +
    geom_point(size = 3, color = "darkblue") +
    geom_errorbar(aes(ymin = mean_resid - 1.96*se_resid,
                      ymax = mean_resid + 1.96*se_resid),
                  width = 0.3, color = "darkblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
    theme_minimal() +
    labs(title = "Mean Residuals Over Time",
         subtitle = "Critical for DiD: Should fluctuate randomly around 0",
         x = "Time Period", y = "Mean Residual")
  
  # Plot 5: Residuals by Parent Geography (if applicable)
  if(!is.null(parent_geo)) {
    parent_summary <- residual_data %>%
      group_by(parent_geo) %>%
      summarise(
        mean_resid = mean(residual),
        sd_resid = sd(residual),
        n = n(),
        .groups = "drop"
      ) %>%
      arrange(mean_resid)
    
    # Show top and bottom 20 parent geographies (or all if fewer)
    n_show <- min(20, nrow(parent_summary))
    parent_to_show <- bind_rows(
      head(parent_summary, n_show/2),
      tail(parent_summary, n_show/2)
    )
    
    plots$parent_geo <- ggplot(parent_to_show, 
                               aes(x = reorder(parent_geo, mean_resid), y = mean_resid)) +
      geom_point(size = 3, color = "steelblue") +
      geom_errorbar(aes(ymin = mean_resid - sd_resid, ymax = mean_resid + sd_resid),
                    width = 0.3, color = "steelblue") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Mean Residuals by", parent_geo),
           subtitle = paste("Showing top/bottom", n_show/2, "geographies"),
           x = parent_geo, y = "Mean Residual")
  }
  
  # Plot 6: Cook's Distance (only if available)
  if(!use_felm) {
    plots$cooks <- ggplot(residual_data, aes(x = seq_along(cooks_d), y = cooks_d)) +
      geom_segment(aes(xend = seq_along(cooks_d), yend = 0), color = "steelblue") +
      geom_point(color = "steelblue", size = 1) +
      geom_hline(yintercept = 4/n_obs, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "orange") +
      theme_minimal() +
      labs(title = "Cook's Distance",
           subtitle = sprintf("Max = %.3f. Values > 4/n (red) or > 1 (orange) are influential", 
                              max_cooks),
           x = "Observation Index", y = "Cook's Distance")
  }
  
  # Plot 7: Histogram of Residuals
  plots$histogram <- ggplot(residual_data, aes(x = residual)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
    stat_function(fun = function(x) dnorm(x, mean = resid_mean, sd = resid_sd) * 
                    n_obs * (max(residual_data$residual) - min(residual_data$residual))/30,
                  color = "darkred", size = 1) +
    theme_minimal() +
    labs(title = "Distribution of Residuals",
         subtitle = "Red curve shows normal distribution for comparison",
         x = "Residuals", y = "Count")
  
  # Plot 8: Residuals vs Covariates (if present)
  if(!is.null(xformla)) {
    covariates <- all.vars(xformla)
    for(covar in covariates) {
      plots[[paste0("resid_", covar)]] <- 
        ggplot(residual_data, aes_string(x = covar, y = "residual")) +
        geom_point(alpha = 0.5, color = "steelblue") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        geom_smooth(se = TRUE, color = "darkred", method = "loess") +
        theme_minimal() +
        labs(title = paste("Residuals vs", covar),
             subtitle = "Pattern suggests non-linear relationship",
             x = covar, y = "Residuals")
    }
  }
  
  # ============================================================================
  # STEP 6: DISPLAY OR SAVE PLOTS
  # ============================================================================
  
  if(!return_plots) {
    cat("Displaying plots...\n")
    print(plots$resid_fitted)
    print(plots$qq)
    print(plots$scale_location)
    print(plots$time)
    if(!is.null(parent_geo)) print(plots$parent_geo)
    if(!use_felm) print(plots$cooks)
    print(plots$histogram)
    
    if(!is.null(xformla)) {
      for(covar in all.vars(xformla)) {
        print(plots[[paste0("resid_", covar)]])
      }
    }
  }
  
  if(save_plots) {
    cat(sprintf("Saving plots to %s...\n", output_dir))
    ggsave(paste0(output_dir, "resid_vs_fitted.png"), plots$resid_fitted, 
           width = 8, height = 6)
    ggsave(paste0(output_dir, "qq_plot.png"), plots$qq, width = 8, height = 6)
    ggsave(paste0(output_dir, "scale_location.png"), plots$scale_location, 
           width = 8, height = 6)
    ggsave(paste0(output_dir, "residuals_time.png"), plots$time, width = 8, height = 6)
    if(!is.null(parent_geo)) {
      ggsave(paste0(output_dir, "residuals_parent_geo.png"), plots$parent_geo,
             width = 8, height = 8)
    }
    if(!use_felm) {
      ggsave(paste0(output_dir, "cooks_distance.png"), plots$cooks, 
             width = 8, height = 6)
    }
    ggsave(paste0(output_dir, "histogram.png"), plots$histogram, width = 8, height = 6)
    
    # Combined plot
    plot_list <- list(plots$resid_fitted, plots$qq, plots$scale_location, plots$time)
    if(!use_felm) plot_list <- c(plot_list, list(plots$cooks))
    plot_list <- c(plot_list, list(plots$histogram))
    
    combined <- do.call(grid.arrange, c(plot_list, ncol = 2))
    ggsave(paste0(output_dir, "combined_diagnostics.png"), combined, 
           width = 12, height = 12)
  }
  
  # ============================================================================
  # STEP 7: CREATE SUMMARY
  # ============================================================================
  
  summary_stats <- list(
    n_observations = n_obs,
    n_units = n_units,
    n_parent_geo = ifelse(!is.null(parent_geo), 
                          length(unique(control_data[[parent_geo]])), NA),
    time_periods = diff(time_range) + 1,
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    resid_mean = resid_mean,
    resid_sd = resid_sd,
    n_outliers = n_outliers,
    pct_outliers = 100*n_outliers/n_obs,
    max_cooks_d = max_cooks,
    shapiro_p = tests$shapiro$p.value,
    bp_p = ifelse(!is.null(tests$bp), tests$bp$p.value, NA),
    dw_stat = ifelse(!is.null(tests$dw), tests$dw$statistic, NA),
    dw_p = ifelse(!is.null(tests$dw), tests$dw$p.value, NA),
    fixed_effects = list(
      time_fe = time_fe,
      unit_fe = unit_fe,
      parent_geo_fe = !is.null(parent_geo)
    )
  )
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  cat("\n=======================================================\n")
  cat("DIAGNOSTICS COMPLETE\n")
  cat("=======================================================\n\n")
  
  cat("KEY TAKEAWAYS:\n")
  cat(sprintf("- Model explains %.1f%% of variance (R-squared)\n", 100*r_squared))
  cat(sprintf("- %.1f%% of observations are outliers (|z| > 2.5)\n", 100*n_outliers/n_obs))
  if(tests$shapiro$p.value < 0.05) {
    cat("- WARNING: Residuals deviate from normality\n")
  }
  if(!is.null(tests$bp) && tests$bp$p.value < 0.05) {
    cat("- WARNING: Evidence of heteroscedasticity (consider robust SE)\n")
  }
  cat("\nCheck residuals over time plot for trends (most important for DiD!)\n\n")
  
  results <- list(
    plots = plots,
    residual_data = residual_data,
    model = outcome_model,
    tests = tests,
    summary = summary_stats
  )
  
  return(invisible(results))
}


# =============================================================================
# EXAMPLE USAGE SCENARIOS
# =============================================================================

# cat("=============================================================================\n")
# cat("EXAMPLE USAGE SCENARIOS\n")
# cat("=============================================================================\n\n")
# 
# # Scenario 1: Basic DiD with time FE only
# cat("# Scenario 1: Basic model (time FE only)\n")
# cat('results1 <- did_outcome_diagnostics(
#   data = mpdta,
#   yname = "lemp",
#   tname = "year",
#   idname = "countyreal",
#   gname = "first.treat",
#   xformla = ~lpop,
#   time_fe = TRUE
# )\n\n')
# 
# # Scenario 2: Census blocks within counties
# cat("# Scenario 2: Nested geography (Census blocks within counties)\n")
# cat('results2 <- did_outcome_diagnostics(
#   data = my_census_data,
#   yname = "outcome",
#   tname = "year",
#   idname = "block_id",
#   gname = "treatment_year",
#   xformla = ~population + income,
#   time_fe = TRUE,
#   parent_geo = "county_id",
#   cluster_var = "county_id"
# )\n\n')
# 
# # Scenario 3: Two-way fixed effects
# cat("# Scenario 3: Two-way fixed effects (unit + time)\n")
# cat('results3 <- did_outcome_diagnostics(
#   data = my_data,
#   yname = "outcome",
#   tname = "year",
#   idname = "unit_id",
#   gname = "treatment_year",
#   time_fe = TRUE,
#   unit_fe = TRUE,
#   use_felm = TRUE  # Recommended for many units
# )\n\n')
# 
# # Scenario 4: All fixed effects
# cat("# Scenario 4: All fixed effects (time + unit + parent geography)\n")
# cat('results4 <- did_outcome_diagnostics(
#   data = my_data,
#   yname = "outcome",
#   tname = "year",
#   idname = "block_id",
#   gname = "treatment_year",
#   xformla = ~population,
#   time_fe = TRUE,
#   unit_fe = TRUE,
#   parent_geo = "county_id",
#   cluster_var = "county_id",
#   use_felm = TRUE
# )\n\n')
  
  # # Run actual example with mpdta
  # data("mpdta")
  # 
  # results <- did_outcome_diagnostics(
  #   data = mpdta,
  #   yname = "lemp",
  #   tname = "year",
  #   idname = "countyreal",
  #   gname = "first.treat",
  #   xformla = ~lpop,
  #   time_fe = TRUE,
  #   return_plots = TRUE
  # )
  # 
  # # Access results
  # cat("\nAccessing results:\n")
  # cat("- results$plots$time          # Most important plot for DiD!\n")
  # cat("- results$plots$resid_fitted  # Check model fit\n")
  # cat("- results$summary             # Summary statistics\n")
  # cat("- results$residual_data       # Full residual dataset\n")
  # cat("- results$model               # Fitted model object\n")

# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# Load example data
# data("mpdta")

# Run diagnostics
# results <- did_outcome_diagnostics(
#   data = mpdta,
#   yname = "lemp",
#   tname = "year",
#   idname = "countyreal",
#   gname = "first.treat",
#   xformla = ~lpop,
#   control_group = "nevertreated",
#   return_plots = TRUE,
#   save_plots = FALSE
# )













