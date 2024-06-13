library(tidyverse)
library(sf)
library(proxy)

setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
getwd()


## Preprocessing the road data ------------------------------------------------

# load in the rki base network
msn_base = st_read("./Interim/MainStreetRoadNetwork/CANADA/mainstreet_allmetrics/geojsons/msn_base_all.geojson") %>%
  select((1:10), per_business_density, per_civic_density, per_employment_density, per_business_independence_index, per_population_change) %>%
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


#--------------------------------------------------------------------------------------------------------------------------------------

#### Processing the Housing Consturctuin Year Data ####

# load in the housing data
setwd("~/cmhc-scaling")
housing_year = read_csv("./Data/Housing_Data.csv") %>%
  select(Key, (2:10)) %>%
  rename("DAUID" = Key) %>%
  mutate(DAUID = as.character(DAUID),
         built_pre1960 = ECYPOCP60,
         built_61_80 = ECYPOC6180,
         built_81_00 = ECYPOC8190 + ECYPOC9100,
         built_01_23 = ECYPOC0105 + ECYPOC0610 + ECYPOC1115 + ECYPOC1621 + ECYPOC22P) %>%
  select(DAUID, (11:14))

housing_year = housing_year %>%
  mutate(
    housing_total = built_pre1960 + built_61_80 + built_81_00 + built_01_23,
    per_pre1960 = built_pre1960 / housing_total * 100,
    per_1960 = built_61_80 / housing_total * 100,
    per_1980 = built_81_00 / housing_total * 100,
    per_2000 = built_01_23 / housing_total * 100
  ) %>%
  select(DAUID, per_pre1960, per_1960, per_1980, per_2000, housing_total)

# attach a spaital component
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
DA = st_read("./Data/lda_000a21a_e") %>%
  st_transform(crs = 3347) %>%
  select(DAUID)

housing = DA %>%
  left_join(housing_year, by = "DAUID")

# export the file into the interim


#### Processing the Housing Type Data  ####

# load in the Housing Type data
setwd("~/cmhc-scaling")
housing_type = read_csv("./Data/Housing_Data.csv") %>%
  select(Key, (11:16)) %>%
  mutate(DAUID = as.character(Key),
         Detached_Housing = ECYSTYSING + ECYSTYSEMI,
         Row = ECYSTYROW,
         Lowrise_Apt = ECYSTYAPU5,
         Highrise_Apt = ECYSTYAP5P,
         Duplex = ECYSTYDUPL) %>%
  select((8:13))

# Get the percentages
housing_type = housing_type %>%
  mutate(
    housing_total = Detached_Housing + Row + Lowrise_Apt + Highrise_Apt + Duplex,
    Detached_Housing = Detached_Housing / housing_total * 100,
    Row = Row / housing_total * 100,
    Lowrise_Apt = Lowrise_Apt / housing_total * 100,
    Highrise_Apt = Highrise_Apt / housing_total * 100,
    Duplex = Duplex / housing_total * 100
  )


# attach a spaital component
setwd("C:/Users/atabascio/CUI/Projects - External - Documents/819. Research & Knowledge Initiative – INFC/3 - Background Data & Research/GIS Map prototype/RKI_MainStreetMatters")
DA = st_read("./Data/lda_000a21a_e") %>%
  st_transform(crs = 3347) %>%
  select(DAUID)

housing_type = DA %>%
  left_join(housing_type, by = "DAUID")


# export the file into the interim







  
## Attach the housing data to the main street scale
msn_base_housing = st_join(msn_base %>% select(id), st_buffer(housing, 1000), join = st_intersects, left = TRUE)

msn_base_housing = msn_base_housing %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  mutate(across(all_of(c("per_pre1960", "per_1960", "per_1980", "per_2000")), ~ weighted.mean(., w = housing_total, na.rm = TRUE))) %>%
  distinct(id, .keep_all = TRUE)




## Defining the reference observation -----------------------------------------

# create a subset based on street ids
selected_roads = c("188450", "187365", "187428", "188525")

# get the percentile average of all variables
initial_case = msn_base %>%
  filter(!is.na(per_business_density) & (id %in% selected_roads)) %>%
  mutate(across(all_of(c(11:64)), ~ mean(., na.rm = TRUE))) %>%
  st_drop_geometry() %>%
  distinct(per_business_density, .keep_all = TRUE)


# remove all the selected road segments from the large base
msn_base =  msn_base%>%
  filter(!id %in% selected_roads)


## Similarity calculation for urban form variables ----------------------------

# Get the population, business and civic density for the large set and case study
initial_case_urban_form = initial_case %>%
  select(per_business_density, per_civic_density, per_population_density) %>%
  st_drop_geometry()

msn_urban_form = msn_base %>%
  select(per_business_density, per_civic_density, per_population_density) %>%
  st_drop_geometry()

# calculate a distance matrix between the msn base set and the initial case study
urban_form_distance = proxy::dist(msn_urban_form, initial_case_urban_form, method = "Euclidean")

# convert to a similarity score
similarity_score = 1 / (1 + urban_form_distance)

similarity_score = as.numeric(similarity_score)

# join back to the msn_base
msn_base = msn_base %>%
  mutate(urban_form_similarity = similarity_score)

# filter the base set based on the similarity of urban form
form_q3 = quantile(msn_base$urban_form_similarity, probs = 0.75)

msn_base_final_set = msn_base %>%
  filter(urban_form_similarity >= form_q3)

st_write(msn_base_final_set, "C:/Users/atabascio/CUI/Projects - External - Documents/829. CMHC Housing on Main Streets/3 - Background Data & Research/Data/scailing_test/ottawa1.geojson", driver = "GeoJSON")


msn_base_final_set %>%
  count(city_name) %>% arrange(desc(n))



## Similarity calculation for Housing Outlook ---------------------------------

# Get the housing construction percentages for the large set and case study
initial_case_housing = initial_case %>%
  select(per_pre1960, per_1960, per_1980, per_2000) %>%
  st_drop_geometry()

msn_housing = msn_base %>%
  select(per_pre1960, per_1960, per_1980, per_2000) %>%
  st_drop_geometry()

# calculate a distance matrix between the msn base set and the initial case study
housing_distance = proxy::dist(msn_housing, initial_case_housing, method = "Euclidean")

# convert to a similarity score
similarity_score = 1 / (1 + housing_distance)

similarity_score = as.numeric(similarity_score)

msn_base = msn_base %>%
  mutate(housing_similarity = similarity_score)

housing_q3 = quantile(msn_base$housing_similarity, probs = 0.75)

msn_base_housing_final_set = msn_base %>%
  filter(housing_similarity >= housing_q3)

# conduct an inner join with the final set

msn_base_final_set = msn_base_final_set %>%
  inner_join(msn_base_housing_final_set %>% select(id) %>% st_drop_geometry(), by = "id")

st_write(msn_base_final_set, "C:/Users/atabascio/CUI/Projects - External - Documents/829. CMHC Housing on Main Streets/3 - Background Data & Research/Data/scailing_test/ottawa2.geojson", driver = "GeoJSON")

msn_base_final_set %>%
  count(city_name) %>% arrange(desc(n))


## Similarity calculation for demographic outlook -----------------------------

# Get the demographic percentages for the large set and case study
initial_case_demo = initial_case %>%
  select(per_population_change) %>%
  st_drop_geometry()

msn_demos = msn_base %>%
  select(per_population_change) %>%
  st_drop_geometry()

# calculate a distance matrix between msn base set and the initial case
demo_distance = proxy::dist(msn_demos, initial_case_demo, method = "Euclidean")

# convert to a similarity score
similarity_score= 1 / (1 + demo_distance)

similarity_score = as.numeric(similarity_score)

msn_base = msn_base %>%
  mutate(demo_similarity = similarity_score)

demo_q3 = quantile(msn_base$demo_similarity, probs = 0.75)

msn_base_demo_final_set = msn_base %>%
  filter(demo_similarity >= demo_q3)

msn_base_final_set = msn_base_final_set %>%
  inner_join(msn_base_demo_final_set %>% select(id) %>% st_drop_geometry(), by = "id")


msn_base_final_set %>%
  count(prname) %>% arrange(desc(n))


# export the final set
st_write(msn_base_final_set, "C:/Users/atabascio/CUI/Projects - External - Documents/829. CMHC Housing on Main Streets/3 - Background Data & Research/Data/scailing_test/test3.geojson", driver = "GeoJSON")
