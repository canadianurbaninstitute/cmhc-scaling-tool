library(tidyverse)
library(sf)
library(cancensus)


# load the similar streets
setwd("C:/Users/atabascio/OneDrive - CUI/Documents/cmhc-scaling")

elice_ave = st_read("./Output/EliceAve/elice_ave_scaled.geojson") %>%
  select(id, city_name, (37:43)) %>%
  mutate(case_study = "elice_ave")

first_ave = st_read("./Output/FirstAve/first_ave_scaled.geojson") %>%
  select(id, city_name, (37:43)) %>%
  mutate(case_study = "first_ave")

lancaster_st = st_read("./Output/LancasterSt/lancaster_st_scaled.geojson") %>%
  select(id, city_name, (37:43)) %>%
  mutate(case_study = "lancaster_st")

montreal_rd = st_read("./Output/MontrealRd/montreal_rd_scaled.geojson") %>%
  select(id, city_name, (37:43)) %>%
  mutate(case_study = "montreal_rd")

all_streets = bind_rows(elice_ave, first_ave, lancaster_st, montreal_rd)

all_streets = all_streets %>%
  distinct(id, .keep_all = TRUE) %>%
  st_transform(crs = 3347)


# load the city data
options(cancensus.api_key = "CensusMapper_47bd3e3bf11f4cc349554e443b695417")

csd_population = get_census(dataset = 'CA21', regions = list(C = "01"),
                            vectors = c('v_CA21_6'),
                            level = 'CSD', use_cache = FALSE, geo_format = "sf")

csd_population = csd_population %>%
  st_transform(crs = 3347)



all_streets_pop = st_intersection(all_streets, csd_population %>% select(name, Population))


all_streets_pop = all_streets_pop %>%
  mutate(City_Size = case_when(
    Population <= 250000 ~ "small",
    Population > 250000 & Population < 1000000 ~ "medium",
    Population >= 1000000 ~ "large"
  ))


streets_summary = all_streets_pop %>%
  st_drop_geometry() %>%
  group_by(City_Size) %>%
  summarise(Surface_Parking = sum(surface_Parking_lots),
            Gas_Stations = sum(gas_stations),
            Anchors = sum(civic_count),
            Residential = sum(Residential),
            Commercial = sum(Commercial),
            Mixed_Use = sum(Mixed.Use))




write.csv(streets_summary, "./site_summary.csv")
