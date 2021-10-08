library(tidyverse)
library(tidycensus)
library(haven)
library(here)
library(sf)
source("routing/analysis_functions.R")

routes <- read_csv(here("routing/data", "all_routes_transit_raw.csv"),
                   col_types = c("geoid_start" = "character",
                                 "geoid_end" = "character")) %>%
  # calculate adjusted duration by multiplying car trips by the 
  # 2019 ratio of INRIX off-peak speed (32) and peak speed (18) for Washington DC
  # https://inrix.com/scorecard-city/?city=Washington%20DC&index=89
  mutate(duration_adj = ifelse(mode == "CAR" & date == "2021-09-15", duration * 1.78, duration))

routes_arl <- routes %>%
  filter(substr(geoid_end, 1, 5) == "51013")
acs <- read_process_acs()
arl_tract <- acs %>%
  select(GEOID) %>%
  

food_sites <- read_dta(here("Retail data", "food_stores_data.dta")) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4269)


tract_food <- st_join(arl_tract, food_sites, join = st_intersects) 

tract_food <- tract_food %>%
  group_by(GEOID) %>%
  summarise(charitable_food = ifelse(sum(charitable_food, na.rm = TRUE) > 0 , 1, 0),
            accepts_snap = ifelse(sum(accepts_snap, na.rm = TRUE) > 0 , 1, 0),
            fresh_produce = ifelse(sum(fresh_produce, na.rm = TRUE) > 0 , 1, 0))


routes_food <- left_join(routes_arl, tract_food, by = c("geoid_end" = "GEOID"))

time_closest <- travel_time_to_closest(routes_food, charitable_food)
