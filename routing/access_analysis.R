library(tidyverse)
library(tidycensus)
library(haven)
library(here)
library(sf)
library(tigris)
source("routing/analysis_functions.R")

routes_transit <- read_csv(here("routing/data", "all_routes_transit_final.csv"),
                   col_types = c("geoid_start" = "character",
                                 "geoid_end" = "character",
                                 "date" = "character")) %>%
  mutate(mode = "TRANSIT")

routes_car <- read_csv(here("routing/data", "all_routes_car_final.csv"),
                           col_types = c("geoid_start" = "character",
                                         "geoid_end" = "character")) 

routes_car_rush <- routes_car %>%
  # calculate adjusted duration by multiplying car trips by the 
  # 2019 ratio of INRIX off-peak speed (32) and peak speed (18) for Washington DC
  # https://inrix.com/scorecard-city/?city=Washington%20DC&index=89
  mutate(adj_duration = adj_duration * 1.78,
         date = "2021-09-15") %>%
  select(-departure_time)

routes_car_wknd <- routes_car %>%
  mutate(date = "2021-09-19") %>%
  select(-departure_time)
  

routes <- rbind(routes_car_rush, routes_car_wknd, routes_transit) 

# routes_arl <- routes %>%
#   filter(substr(geoid_end, 1, 5) == "51013")

acs <- read_process_acs()
  
  
routes_wide <- routes %>%
  select(geoid_end, geoid_start, adj_duration, mode, date) %>%
  #mutate(mode = factor(mode)) %>%
  pivot_wider(id_cols = c(geoid_start, geoid_end, date),
              names_from = mode, 
              values_from = adj_duration)

routes_self_wkdy <- tibble(
  geoid_start = acs$GEOID,
  geoid_end = acs$GEOID,
  CAR = 0,
  TRANSIT = 0,
  date = "2021-09-15"
  
)

routes_self_wknd <- tibble(
  geoid_start = acs$GEOID,
  geoid_end = acs$GEOID,
  CAR = 0,
  TRANSIT = 0,
  date = "2021-09-19"
  
)

routes_wide <- rbind(routes_wide, routes_self_wkdy, routes_self_wknd)

routes_acs <- routes_wide %>%
  left_join(acs, by = c("geoid_start" = "GEOID")) %>%
  mutate(CAR = as.numeric(CAR),
         TRANSIT = as.numeric(TRANSIT),
         wt_duration = CAR * (1 - pct_no_car/100) + TRANSIT * (pct_no_car/100))

food_sites <- read_csv(here("Final food data", "Food_retailers_TRANSPORT.csv")) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4269)

va_tract <- tracts(state = "51")
tract_food <- st_join(va_tract, food_sites, join = st_intersects)

tract_food <- tract_food %>%
  mutate(is_snap = case_when(location_type == "SNAP-retailer" ~ 1,
                             T ~ 0),
         is_charitable = case_when(location_type == "Charitable food-site" ~ 1,
                                   T ~ 0),
         char_open = case_when(is_charitable == 1 & eligibility_requirement == 0 ~ 1,
                               T ~ 0),
         char_child = case_when(is_charitable == 1 & age_restrictions_for_children_or == 1 ~ 1,
                                T ~ 0),
         char_sen = case_when(is_charitable == 1 & age_restrictions_for_seniors_onl == 1 ~ 1,
                              T ~ 0)
  )

tract_food_count <- tract_food %>%
  group_by(GEOID) %>%
  summarise(
    count_snap = sum(is_snap, na.rm = TRUE),
    count_char = sum(is_charitable, na.rm = TRUE),
    count_char_child = sum(char_child, na.rm = TRUE),
    count_char_sen = sum(char_sen, na.rm = TRUE),
  )

routes_all <- routes_acs %>%
  left_join(tract_food_count, by = c("geoid_end" = "GEOID"))

write_csv(routes_all, here("routing/data", "routes_all.csv"))

time_closest <- travel_time_to_closest(all_data = routes_all, 
                                       food_type = count_snap,
                                       dur_type = wt_duration,
                                       route_date = "2021-09-15")
