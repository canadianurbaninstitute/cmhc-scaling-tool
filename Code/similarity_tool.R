## Title: housing on main streets - similarity street tool
## Author: Alex Tabascio
## Date: 2024-06-17
## Summary: This script contains all the tools to recreate the process done in the
## housing on main streets research report for any main street in the country

## Tools

# similar_streets - this function produces a spatial object of similar streets given a list of main street
# segments and the housing data created from the housing_data_setup.r script

# get_casestudy_units - this function returns the location and unit count of every residential area within
# 250 metres given a list of street segments

# scale_housing_stock - this function returns an estimated total of units for all similar streets based on the 
# proposed percentage increase.

# more info on data and methodology can be found in the READ ME

library(tidyverse)
library(sf)
library(proxy)
library(units)

getwd()
options(scipen = 999)



# ------------------------------------------------------------------------------------------------

#### SIMILARITY SCORE FUNCTION ####

# this function will take in a list of road segments and output a spatial data frame
# of similar streets

similar_streets = function(road_network, list_of_streets){
  
  # calculate the average values of the study area for the variables that will
  # be used in the similarity calculation
  
  initial_roads = road_network %>%
    filter(!is.na(per_business_density) & id %in% list_of_streets)
  
  initial_roads = initial_roads %>%
    mutate(across(all_of(c(11:27)), ~ mean(., na.rm = TRUE))) %>%
    st_drop_geometry() %>%
    distinct(per_business_density, .keep_all = TRUE)
  
  
  # remove the control roads from the larger sample
  test_roads = road_network %>%
    filter(!id %in% list_of_streets) %>%
    st_drop_geometry()
  
  # calculate the similarity metric for each of the four categories
  # Urban form - per_population_density / per_employment_density / per_population_change
  # Street Content - per_business_density / per_civic_density / per_business_independence_index / surface_parking
  # Housing Stock - per_pre1960 / per_61_00 / per_00_23 / Detached_Housing / Mid_Density / Highrise_Apt
  # Demographics - per_indigenous / per_visible_minorities / per_immigrants_non_permanent_residents / 
  # per_average_employment_income
  
  # urban form
  urban_form_dist = proxy::dist(test_roads %>% select(per_population_density, per_population_change, per_employment_density),
                                initial_roads %>% select(per_population_density, per_population_change, per_employment_density),
                                method = "Euclidean")
  
  urban_form_score = 1 / (1 + urban_form_dist)
  urban_form_score = as.numeric(urban_form_score)
  
  # Street Content
  street_dist = proxy::dist(test_roads %>% select(per_business_density, per_civic_density, per_business_independence_index,
                                                  surface_Parking), 
                            initial_roads %>% select(per_business_density, per_civic_density, per_business_independence_index,
                                                    surface_Parking),
                            method = "Euclidean")
  
  street_score = 1 / (1 + street_dist)
  street_score = as.numeric(street_score)
  
  # Housing Stock
  housing_dist = proxy::dist(test_roads %>% select(per_pre1960, per_61_00, per_00_23, Detached_Housing, Mid_Density, Highrise_Apt),
                             initial_roads %>% select(per_pre1960, per_61_00, per_00_23, Detached_Housing, Mid_Density, Highrise_Apt),
                             method = "Euclidean")
  
  housing_score = 1 / (1 + housing_dist)
  housing_score = as.numeric(housing_score)
  
  # Demographic
  demo_dist = proxy::dist(test_roads %>% select(per_indigenous, per_visible_minorities, per_immigrants_non_permanent_residents,
                                                per_average_employment_income),
                          initial_roads %>% select(per_indigenous, per_visible_minorities, per_immigrants_non_permanent_residents,
                                                   per_average_employment_income),
                          method = "Euclidean")
  
  demo_score = 1 / (1 + demo_dist)
  demo_score = as.numeric(demo_score)
  
  
  # combine all metrics and take the average
  score_summary = tibble(urban_form = unlist(urban_form_score),
                         street_content = unlist(street_score),
                         housing = unlist(housing_score),
                         demos = unlist(demo_score))
  
  score_summary = score_summary %>%
    mutate(sim_index =  rowMeans(select(., urban_form:demos), na.rm = TRUE))
  
  
  # join the scores back to the test scores
  test_roads = bind_cols(test_roads, score_summary)
  
  # filter all roads where the average in above a threshold of 0.75
  final_set = test_roads %>%
    filter(sim_index >= 0.75)
  
  
  final_set = road_network %>%
    select(id) %>%
    inner_join(final_set, by = "id")
  
  # return the final set
  return(final_set)
}

# -----------------------------------------------------------------------------------------------------

#### BASE HOUSING COUNT FUNCTION ####

get_casestudy_units = function(road_network, location_data, list_of_streets){
  
  # filter the road network based on the list of streets
  initial_roads = road_network %>%
    filter(!is.na(per_business_density) & id %in% list_of_streets)
  
  # turn the location data as a sptail data frame
  location_data = location_data %>%
    filter(!is.na(reppoint_latitude))
  location_data_st = st_as_sf(location_data, coords = c("reppoint_longitude", "reppoint_latitude"), crs = 4326) %>%
    st_transform(crs = 3347)
  
  # concatenate the address field
  location_data_st = location_data_st %>%
    mutate(
      civic_no = as.character(civic_no),
      civic_no = ifelse(civic_no == "", NA, civic_no),
      official_street_name = ifelse(official_street_name == "", NA, official_street_name),
      official_street_type = ifelse(official_street_type == "", NA, official_street_type),
      official_street_dir = ifelse(official_street_dir == "", NA, official_street_dir),
      # combine the address
      Address = str_c(
        if_else(!is.na(civic_no), str_c(civic_no), ""),
        if_else(!is.na(official_street_name), str_c(" ", official_street_name), ""),
        if_else(!is.na(official_street_type), str_c(" ", official_street_type), ""),
        if_else(!is.na(official_street_dir), str_c(" ", official_street_dir), ""),
        sep = ""
      )
    ) %>%
    select(loc_guid, addr_guid, Address, bu_use)
  
  
  # create an intersection between the buffered roads and the location data
  location_data_clipped = st_intersection(st_buffer(initial_roads %>% select(), 250), location_data_st)
  
  # with the initial data clipped preform a nearest neighbour join for the road segment id
  case_study_locations = st_join(location_data_clipped, initial_roads, join = st_nearest_feature)
  
  # get the summary of units for each address
  # used for internal testing
  address_summary = case_study_locations %>%
    group_by(id, Address) %>%
    summarise(units = n())
  
  # return the address points
  return(address_summary)
  
}



















# ---------------------------------------------------------------------------------------------------------------------------


# load in msn_base data
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
msn_base = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_allmetrics/geojsons/msn_base_all.geojson") %>%
  select((1:10), per_business_density, per_civic_density, per_employment_density, per_business_independence_index,
         per_population_change, per_population_density, per_visible_minorities, per_indigenous, per_immigrants_non_permanent_residents,
         per_average_employment_income) %>%
  filter(!is.na(per_population_change)) %>%
  st_transform(crs = 3347)


# load in the address and location data for Manitoba
setwd("~/cmhc-scaling")
location_data = read.csv("./Data/address_data/joined_addresses_46.csv") %>%
  select((2:9), bu_use, reppoint_latitude, reppoint_longitude, -civic_no_suffix) %>%
  filter(bu_use == 1 | bu_use == 2)


# create a subset based on street ids
montreal_rd = c("118659", "188525")

elice_ave = c("95731", "95779")





