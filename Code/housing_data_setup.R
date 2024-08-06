## Title: housing on main streets - similarity street tool
## Author: Alex Tabascio
## Date: 2024-06-17
## Summary: This script preforms the outlined methodology used to identify similar streets to the
## case studies outlined within the CMHC housing on main streets project based on four main pillars
## Urban form, Street Content, Housing Stock, and Demographics



library(tidyverse)
library(sf)
library(proxy)
library(units)

setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
getwd()
options(scipen = 999)


## Preprocessing the road data ------------------------------------------------

# load in the rki base network
msn_base = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_allmetrics/geojsons/msn_base_all.geojson") %>%
  select((1:10), business_count, civic_count, per_business_density, per_civic_density, per_employment_density, per_business_independence_index,
         per_population_change, per_population_density, per_visible_minorities, per_indigenous, per_immigrants_non_permanent_residents,
         per_average_employment_income) %>%
  filter(!is.na(per_population_change)) %>%
  st_transform(crs = 3347)

# load in the identified main streets
high_density = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_base/msn_highdensity.shp") %>%
  rename("id" = ROADSEGID)
low_density = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_base/msn_lowdensity.shp") %>%
  rename("id" = ROADSEGID)

main_streets = bind_rows(high_density, low_density)
main_streets = main_streets %>%
  distinct(id, .keep_all = TRUE)

# get a subset of only main streets within the base network to better fit the project parameters
msn_base = msn_base %>%
  semi_join(st_drop_geometry(main_streets), by = "id")

rm(high_density, low_density, main_streets)


#--------------------------------------------------------------------------------------------------------------------------------------

#### Processing the Housing Construction Year Data ####

# load in the housing data
setwd("~/cmhc-scaling")
housing_year = read_csv("./Data/Housing_Data.csv") %>%
  select(Key, (2:10)) %>%
  rename("DAUID" = Key) %>%
  mutate(DAUID = as.character(DAUID),
         built_pre1960 = ECYPOCP60,
         built_61_00 = ECYPOC6180 + ECYPOC8190 + ECYPOC9100,
         built_01_23 = ECYPOC0105 + ECYPOC0610 + ECYPOC1115 + ECYPOC1621 + ECYPOC22P) %>%
  select(DAUID, (11:13))

housing_year = housing_year %>%
  mutate(
    housing_total = built_pre1960 + built_61_00 +  built_01_23,
    per_pre1960 = built_pre1960 / housing_total * 100,
    per_61_00 = built_61_00 / housing_total * 100,
    per_00_23 = built_01_23 / housing_total * 100
  ) %>%
  select(DAUID, per_pre1960, per_61_00, per_00_23)


#### Processing the Housing Type Data  ####

# load in the Housing Type data
setwd("~/cmhc-scaling")
housing_type = read_csv("./Data/Housing_Data.csv") %>%
  select(Key, (11:16)) %>%
  mutate(DAUID = as.character(Key),
         Detached_Housing = ECYSTYSING + ECYSTYSEMI,
         Mid_Density = ECYSTYROW + ECYSTYAPU5 + ECYSTYDUPL,
         Highrise_Apt = ECYSTYAP5P) %>%
  select((8:11))

# Get the percentages
housing_type = housing_type %>%
  mutate(
    housing_total = Detached_Housing + Mid_Density + Highrise_Apt,
    Detached_Housing = Detached_Housing / housing_total * 100,
    Mid_Density = Mid_Density / housing_total * 100,
    Highrise_Apt = Highrise_Apt / housing_total * 100,
  )


# join the housing construction year and housing type 
housing_vars = housing_year %>%
  left_join(housing_type, by = "DAUID")



# attach a spatial component
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
DA = st_read("./Data/lda_000a21a_e") %>%
  st_transform(crs = 3347) %>%
  select(DAUID)

housing = DA %>%
  left_join(housing_vars, by = "DAUID")

## Attach the housing data to the main street scale
# according to the main street proposals from the case study opportunities were found up to 250 metres from the
# defined road segment
msn_base_housing = st_join(msn_base %>% select(id), st_buffer(housing, 250), join = st_intersects, left = TRUE)

msn_base_housing = msn_base_housing %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(across(all_of(c("per_pre1960", "per_61_00", "per_00_23",
                         "Detached_Housing", "Mid_Density", "Highrise_Apt")), ~ weighted.mean(., w = housing_total, na.rm = TRUE))) %>%
  distinct(id, .keep_all = TRUE)


# export the file into the interim
# load in the Housing Type data
setwd("~/cmhc-scaling")
write.csv(housing_vars, "./Interim/housing_vars.csv")


rm(housing_type, housing_vars, housing_year, housing)


# ----------------------------------------------------------------------------------------------------------

#### Processing the Surface Parking Data ####

# load in the surface parking
setwd("~/cmhc-scaling")
surface_parking = st_read("./Data/parking_sf/canada_parking.geojson") %>%
  select(osm_id) %>%
  st_transform(crs = 3347)

# preform an intersection with the main street network
msn_base_parking = st_intersection(st_buffer(msn_base %>% select(id), 250), surface_parking)

# get the sum of surface parking area
msn_base_parking = msn_base_parking %>%
  mutate(area = st_area(geometry),
         area = drop_units(area)) %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(surface_Parking_lots = sum(n()),
            surface_Parking_area = sum(area))


# combine all the datasets together

msn_base_final = msn_base %>%
  left_join(msn_base_housing %>% select(-DAUID, -housing_total) %>% st_drop_geometry(), by = "id")

msn_base_final = msn_base_final %>%
  left_join(msn_base_parking, by = "id") %>%
  mutate(surface_Parking_area = replace_na(surface_Parking_area, 0),
         surface_Parking_lots = replace_na(surface_Parking_lots, 0))


# convert all the added columns into percentiles
msn_base_final = msn_base_final %>%
  mutate(per_pre1960 = cume_dist(per_pre1960),
         per_61_00 = cume_dist(per_61_00),
         per_00_23 = cume_dist(per_00_23),
         Detached_Housing = cume_dist(Detached_Housing),
         Mid_Density = cume_dist(Mid_Density),
         Highrise_Apt = cume_dist(Highrise_Apt),
         per_surface_Parking_lots = cume_dist(surface_Parking_lots),
         per_surface_Parking_area = cume_dist(surface_Parking_area))


# -----------------------------------------------------------------------------------------------------------

#### Processing the Gas Station Data ####

# load in the national business data and filter out Gas Stations
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
gas_stations = st_read("./Interim/BusinessAndCivic/all")
naics_code = read_csv("./Data/EA_Data_Export/Businesses_Canada.csv") %>%
  select(`Unique Identifier`, `NAICS Code - 4 Digit`) %>%
  rename("Uni_Id" = `Unique Identifier`, "NAICS_4" = `NAICS Code - 4 Digit`)

gas_stations = gas_stations %>%
  inner_join(naics_code, by = "Uni_Id")

# filter out all gas stations
gas_stations = gas_stations %>%
  filter(NAICS_4 == 4471)

# preform an intersection with the main street network
msn_base_gas = st_intersection(st_buffer(msn_base %>% select(id), 250), gas_stations)

# get the sum of surface parking area
msn_base_gas = msn_base_gas %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(gas_stations = sum(n()))

# combine to the other data sets
msn_base_final = msn_base_final %>%
  left_join(msn_base_gas, by = "id") %>%
  mutate(gas_stations = replace_na(gas_stations, 0))



# export the final housing data
st_write(msn_base_final, "./Interim/cmhc_ms_base.geojson", driver = "GeoJson")










