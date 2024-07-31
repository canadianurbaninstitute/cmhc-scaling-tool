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

# scale_housing - this function returns an estimated total of units for all similar streets based on the 
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

# -----------------------------------------------------------------------------------------------

#### BASE HOUSING COUNT FUNCTION ####

get_casestudy_units = function(road_network, location_data, list_of_streets){
  
  # filter the road network based on the list of streets
  initial_roads = road_network %>%
    filter(!is.na(per_business_density) & id %in% list_of_streets)
  
  # turn the location data as a spatial data frame
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
  
  roads_buffer = st_buffer(initial_roads, 250)
  roads_buffer = st_union(roads_buffer)
  
  # create an intersection between the buffered roads and the location data
  location_data_clipped = location_data_st[st_intersects(location_data_st, roads_buffer, sparse = FALSE), ]
  
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



# -----------------------------------------------------------------------------------------------

#### HOUSING SCALING FUNCTION ####

scale_housing = function(similarity_network, proposed_increase){
  
  # get the provincial codes for each similar street
  provincial_codes = unique(similarity_network$pruid)
  
  # for each province within the similarity network
  for(i in 1:length(provincial_codes)){
    # filter the similarity network for each province
    similarity_network_filter = similarity_network %>%
      filter(pruid == provincial_codes[i])
    
    similarity_network_ids = unique(similarity_network_filter$id)
    
    # load the location data based on the province
    file_directory = paste0("./Data/address_data/joined_addresses_", provincial_codes[i], ".csv")
    location_data_prov = read.csv(file_directory) %>%
      select((2:9), bu_use, reppoint_latitude, reppoint_longitude, -civic_no_suffix) %>%
      filter(bu_use == 1 | bu_use == 2)
    
    # use the get_casestudy_units functions to get the count of units for each province
    provincial_units = get_casestudy_units(similarity_network, location_data_prov, similarity_network_ids)
    
    # summarise the address by road id and attach the road variables back
    provincial_units = provincial_units %>%
      st_drop_geometry() %>%
      group_by(id) %>%
      summarise(base_units = sum(units))
    
    prov_road_summary = similarity_network_filter %>%
      left_join(provincial_units, by = "id") %>%
      mutate(base_units = replace_na(base_units, 0))
    
    # bind the data based on the iteration
    if(i == 1){
      all_roads = prov_road_summary
    } else {
      all_roads = bind_rows(all_roads, prov_road_summary)
      
    }
  }
  
  # adjust the base_housing depending on the proposed increase
  percentage = 1 + (proposed_increase / 100)
  
  all_roads_adjusted = all_roads %>%
    mutate(adjuted_units = base_units * percentage)
  
  
  # return the combine data set
  return(all_roads_adjusted)
  
}













# ---------------------------------------------------------------------------------------------------------------------------

# load in the housing variables
setwd("~/cmhc-scaling")
cmhc_base = st_read("./Interim/cmhc_ms_base.geojson") %>%
  st_transform(crs = 3347)

unit_data = read_csv("./Data/address_data/joined_addresses_35.csv")



# create a subset based on street ids
montreal_rd = c("118659", "188525", "187365", "188450")

elice_ave = c("79466", "79859", "95779")

lancaster_st = c("197566")

first_ave = c("31757")




units_count = get_casestudy_units(cmhc_base, unit_data, montreal_rd)
st_write(units_count, "./Output/montreal_rd_units.geojson")


sim_streets = similar_streets(cmhc_base, montreal_rd)
st_write(montreal_rd_sim, "./Output/montreal_rd_sim.geojson")

montreal_rd_scaled = scale_housing(montreal_rd_sim, 20)
st_write(montreal_rd_scaled, "./Output/montreal_rd_scaled.geojson")





