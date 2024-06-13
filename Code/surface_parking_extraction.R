library(osmextract)
library(sf)
library(tidyverse)

pbf_file_path = "surface_parking/yukon.osm.pbf"

osm_data = oe_read(
  pbf_file_path,
  layer = "multipolygons",
  extra_tags = c("amenity", "parking"))

parking = osm_data %>%
  filter(amenity == "parking" & parking == "surface") %>%
  select(osm_id, amenity, parking) %>%
  st_transform(crs = 3347)

st_write(parking, "parking_sf/yukon.geojson", driver = "GeoJSON")



# load in and combine all the provincial files

alberta = st_read("./parking_sf/alberta.geojson")
britishColumbia = st_read("./parking_sf/britishColumbia.geojson")
manitoba = st_read("./parking_sf/manitoba.geojson")
newBrunswick = st_read("./parking_sf/newBrunswick.geojson")
newfoundland = st_read("./parking_sf/newfoundlandAndLabrador.geojson")
nwt = st_read("./parking_sf/northwestTerritories.geojson")
novascotia = st_read("./parking_sf/novaScotia.geojson")
nunavut = st_read("./parking_sf/nunavut.geojson")
ontario = st_read("./parking_sf/ontario.geojson")
pei = st_read("./parking_sf/pei.geojson")
quebec = st_read("./parking_sf/quebec.geojson")
saskatchewan = st_read("./parking_sf/saskatchewan.geojson")
yukon = st_read("./parking_sf/yukon.geojson")

canada_parking = bind_rows(alberta, britishColumbia, manitoba, newBrunswick, newfoundland, novascotia, nunavut,
                           nwt, ontario, pei, quebec, saskatchewan, yukon)



st_write(canada_parking, "parking_sf/canada_parking.geojson")