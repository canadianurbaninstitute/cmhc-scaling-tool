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
                                                  per_surface_Parking_area), 
                            initial_roads %>% select(per_business_density, per_civic_density, per_business_independence_index,
                                                     per_surface_Parking_area),
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
    filter(!is.na(reppoint_latitude) & bu_use != 4)
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
    summarise(units = n(),
              bu_use = round(mean(bu_use), 1))
  
  # readjust if the mean doesn't equal a whole number to mixed use
  address_summary = address_summary %>%
    mutate(bu_use = (case_when(
      bu_use == 1.0 ~ 1.0,
      bu_use == 2.0 ~ 2.0,
      bu_use == 3.0 ~ 3.0,
      TRUE ~ 3.0
    )))
  
  
  # return the address points
  return(address_summary)
  
}



# -----------------------------------------------------------------------------------------------

#### HOUSING SCALING FUNCTION ####

scale_housing = function(similarity_network){
  
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
      filter(bu_use != 4)
    
    # use the get_casestudy_units functions to get the count of units for each province
    provincial_units = get_casestudy_units(similarity_network, location_data_prov, similarity_network_ids)
    
    
    # Add the logic for the Low-Density Sites
    provincial_units = provincial_units %>%
      mutate(LD_site_conversion = case_when(
        # SDH
        bu_use == 1 & units <= 1 ~ "Residential",
        # Commercial
        bu_use == 2 & units <= 2 ~ "Commercial",
        # Mixed Use
        bu_use == 3 & units <= 3 ~ "Mixed-Use",
        TRUE ~ "Not-Eligible"
      ))
    
    # remove residential address where units are in the same location
    residential_conversions = provincial_units %>%
      filter(LD_site_conversion == "Residential" | LD_site_conversion == "Commercial" | LD_site_conversion == "Mixed-Use")
    
    duplicate_pairs = st_equals(residential_conversions)
    
    # Filter out pairs where indices are equal (same point compared to itself)
    duplicate_pairs = duplicate_pairs[lengths(duplicate_pairs) > 1]
    
    # Create a list of all duplicate IDs
    duplicate_ids = unique(unlist(lapply(duplicate_pairs, function(x) residential_conversions$Address[x])))
    
    # Filter out duplicates from the original sf dataframe
    provincial_units = provincial_units %>% filter(!Address %in% duplicate_ids)
    
    # summarise the address by road id and attach the road variables back
    provincial_units = provincial_units %>%
      st_drop_geometry() %>%
      group_by(id) %>%
      count(LD_site_conversion) %>%
      pivot_wider(names_from = "LD_site_conversion", values_from = "n") 
  
    columns_to_replace = c("Commercial", "Mixed-Use", "Not-Eligible", "Residential")
      
    prov_road_summary = similarity_network_filter %>%
      left_join(provincial_units, by = "id") %>%
      mutate(across(all_of(columns_to_replace), ~ replace_na(., 0)))
    
    # bind the data based on the iteration
    if(i == 1){
      all_roads = prov_road_summary
    } else {
      all_roads = bind_rows(all_roads, prov_road_summary)
      
    }
  }
  
  # get the scaling total by adding all civic locations, parking lots,
  # gas stations and Low Density Conversions
  all_roads = all_roads %>%
    rowwise() %>%
    mutate(total_sites = sum(c_across(c(civic_count, surface_Parking_lots, gas_stations,
                                        Commercial, `Mixed-Use`, Residential))))
  
  
  columns = c("civic_count", "surface_Parking_lots", "gas_stations", "Commercial",
              "Mixed-Use", "Residential", "total_sites")
  
  
  all_roads = all_roads %>%
    select(-all_of(columns), everything(), all_of(columns))
  
  # return the combine data set
  return(all_roads)
  
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

lakeshore = c("190920")



# unit count for case studies
units_count = get_casestudy_units(cmhc_base, unit_data, lakeshore)
st_write(units_count, "./Output/lakeshore_units.geojson")

# site count for case study
case_study_street = cmhc_base %>%
  subset(id %in% montreal_rd)
case_study_street_montreal_rd = scale_housing(case_study_street)
write.csv(case_study_street_montreal_rd %>% st_drop_geometry(), "./Output/montreal_rd_site_count.csv")

# scaling tool
sim_streets = similar_streets(cmhc_base, montreal_rd)

scaled_streets_MontrealRd = scale_housing(sim_streets)
st_write(scaled_streets_MontrealRd, "./Output/montreal_rd_scaled.geojson")

scaled_streets_MontrealRd = scaled_streets_MontrealRd %>%
  st_drop_geometry() %>%
  select((1:9), civic_count, surface_Parking_lots, gas_stations,
         Commercial, `Mixed-Use`, Residential, total_sites) %>%
  mutate(case_study = "Montreal Rd")


write.csv(scaled_streets_MontrealRd, "./Output/montreal_rd_scaled.csv")


# Get number for all case studies
All_case_studies = bind_rows(case_study_street_elice_ave, case_study_street_first_ave, case_study_street_lancaster_st, case_study_street_montreal_rd)

write.csv(All_case_studies %>% st_drop_geometry(), "./Output/All_CaseStudies.csv")


# Get numbers for all similar streets
All_sim_streets = bind_rows(scaled_streets_EliceAve, scaled_streets_FirstAve, scaled_streets_LancasterSt, scaled_streets_MontrealRd)

All_sim_streets = All_sim_streets %>%
  distinct(id, .keep_all = TRUE)

write.csv(All_sim_streets %>% st_drop_geometry(), "./Output/All_SimStreets.csv")
