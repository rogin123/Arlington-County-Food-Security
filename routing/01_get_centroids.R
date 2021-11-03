library(tidyverse)
library(here)
library(sf)
library(tigris)
library(urbnmapr)
library(rjson)

#https://mcdc.missouri.edu/applications/geocorr2014.html
#run for DC, MD, VA - select tracts as source and target geographies
centroids_all <- read_csv(here("routing/data", "geocorr_centroids.csv")) %>%
  filter(tract != "Tract") %>%
  st_as_sf(coords = c("intptlon", "intptlat"), crs = 4269, remove = FALSE) %>%
  mutate(tract_str = str_replace(tract, "[.]", ""),
    geoid = paste0(county, tract_str)) %>%
  rename(c("lon" = "intptlon", "lat" = "intptlat")) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

# read in food sites to subset to tracts with food sites
# food_sites <- read_dta(here("Retail data", "food_stores_data.dta")) %>%
#   st_as_sf(coords = c("longitude", "latitude")) %>%
#   st_set_crs(4269)

food_sites <- read_csv(here("Final food data", "Food_retailers_TRANSPORT.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4269)

va_tract <- tracts(state = "51")
# md_tract <- tracts(state = "24")
# dc_tract <- tracts(state = "11")
# all_tract <- rbind(va_tract, md_tract, dc_tract)

tract_food <- st_join(va_tract, food_sites, join = st_intersects) 
unique_tract <- tract_food %>%
  filter(!is.na(objectid)) %>%
  pull(GEOID) %>%
  unique()

# centroids_food <- centroids_all %>%
#   filter(geoid %in% unique_tract)

# Route to all tracts in arlington, alexandria, and fairfax
centroids_food <- centroids_all %>%
  filter(cntyname %in% c("Arlington VA", "Alexandria city VA", "Fairfax VA"))

centroids_arl <- centroids_all %>%
  # exclude two tracts with zero population and tract from analysis and 
  # tract inside Joint Base Myer-Henderson Hall as OTP fails to find any routes 
  # due to lack of public access
  filter(cntyname == "Arlington VA" & !(tract %in% c("9801.00", "9802.00", "1034.01")))

route_pairs <- st_join(centroids_arl,
                       centroids_food, 
                       join = st_is_within_distance,
                       #30 miles is 48,280 meters
                       dist = 25000,
                       suffix = c("_start", "_end"))

route_out <- route_pairs 
st_geometry(route_out) <- NULL
write_csv(route_out, here("routing/data", "route_pairs.csv"))
  
route_out <- route_out %>%
  mutate(state_end = substr(geoid_end, 1, 2))

va <- counties(state = "51")
md <- counties(state = "24")
dc <- counties(state = "11")
all_counties <- rbind(va, md, dc)

end_point <- centroids_all %>% 
  filter(geoid %in% unique(route_pairs$geoid_end))

end_counties <- unique(route_pairs$cntyname_end)

ggplot(data = all_counties) +
  geom_sf() +
  geom_sf(data = end_point)


#Create json with bounds for clipping osm
bbox <- st_bbox(end_point)

bounding <- list(
  n = bbox[["ymax"]] + 0.25,
  s = bbox[["ymin"]] - 0.25,
  e = bbox[["xmin"]] - 0.25,
  w = bbox[["xmax"]] + 0.25
  
)

json_bounding <- toJSON(bounding)
write(json_bounding, here("routing/data", "osm_bounds.json"))
