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

#### CREATING THE SIMILARITY SCORE FUNCTION ####

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
  
  # return the final set
  return(final_set)
}

# -----------------------------------------------------------------------------------------------------










# create a subset based on street ids
montreal_rd = c("118659", "188525")

elice_ave = c("95731", "95779")


final_montreal_rd = similar_streets(msn_base_final, montreal_rd)

final_elice_ave = similar_streets(msn_base_final, elice_ave)


final_elice_ave = msn_base %>%
  select(id) %>%
  inner_join(final_elice_ave, by = "id")

st_write(final_elice_ave, "./Ouput/final_elice_ave.geojson", driver = "GeoJSON")



