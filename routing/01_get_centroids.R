library(tidyverse)
library(here)
library(sf)
library(tigris)
library(urbnmapr)
library(rjson)

#https://mcdc.missouri.edu/applications/geocorr2014.html
centroids_all <- read_csv(here("routing/data", "geocorr_centroids.csv")) %>%
  filter(tract != "Tract") %>%
  st_as_sf(coords = c("intptlon", "intptlat"), crs = 4269, remove = FALSE) %>%
  mutate(tract_str = str_replace(tract, "[.]", ""),
    geoid = paste0(county, tract_str)) %>%
  rename(c("lon" = "intptlon", "lat" = "intptlat")) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))


centroids_arl <- centroids_all %>%
  filter(cntyname == "Arlington VA")


route_pairs <- st_join(centroids_arl,
                       centroids_all, 
                       join = st_is_within_distance,
                       #30 miles is 48,280 meters
                       dist = 25000,
                       suffix = c("_start", "_end"))

route_out <- route_pairs 
st_geometry(route_out) <- NULL
write_csv(route_out, here("routing/data", "route_pairs.csv"))
  

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
