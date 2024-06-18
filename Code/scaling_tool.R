## Title: housing on main streets - scaling tool
## Author: Alex Tabascio
## Date: 2024-06-17
## Summary: This script takes the cleaned address and location data from the National Address
## Register to create a realistic estimate of new housing stock across the country based on the
## proposed changes of the defined case studies


## Load Libraries
library(tidyverse)
library(sf)


# run a test of elice avenue

elice_ave = c("95731", "95779")

setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")

# load in the elice avenue road segments
road_network = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_allmetrics/geojsons/msn_base_all.geojson") %>%
  select((1:10), per_business_density, per_civic_density, per_employment_density, per_business_independence_index,
         per_population_change, per_population_density, per_visible_minorities, per_indigenous, per_immigrants_non_permanent_residents,
         per_average_employment_income) %>%
  filter(!is.na(per_population_change)) %>%
  st_transform(crs = 3347)

initial_roads = road_network %>%
  filter(!is.na(per_business_density) & id %in% elice_ave)

# load in the address and location data for Manitoba
setwd("~/cmhc-scaling")
location_data = read.csv("./Data/address_data/joined_addresses_46.csv") %>%
  select((2:9), bu_use, reppoint_latitude, reppoint_longitude, -civic_no_suffix) %>%
  filter(bu_use == 1 | bu_use == 2)

location_data = location_data %>%
  filter(!is.na(reppoint_latitude))
location_data_st = st_as_sf(location_data, coords = c("reppoint_longitude", "reppoint_latitude"), crs = 4326) %>%
  st_transform(crs = 3347)


# concatenate the address field using str_c and coalesce
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



# --------------------------------------------------------------------------------------------------------------------

# get the summary of units for each address
# used for internal testing
address_summary = case_study_locations %>%
  st_drop_geometry() %>%
  group_by(id, Address) %>%
  summarise(units = n())

address_summary = initial_roads %>%
  select((1:9)) %>%
  right_join(address_summary, by = "id")


# export the final shapefile
st_write(address_summary, "./Output/elice_ave_units.geojson", driver = "GeoJSON")







