read_process_acs <- function() {
  ## Uses tidycensus::get_acs function to query API and obtain ACS estimates
  ## for defined variables. Reshapes data frame to wide.
  
  acs = get_acs(state = "51", county = "013", geography = "tract", 
                variables = c("B02001_003", "B02001_002", "B01003_001",
                              "DP04_0001", "B03003_003", "B02001_005", "C17002_001",
                              "C17002_002", "C17002_003", "DP04_0058P",
                              "B08201_001", "B08201_002", "B23025_005",
                              "S0801_C01_001", "S0801_C01_002",
                              "B01001_003", "B01001_004", "B01001_005",
                              "B01001_006", "B01001_020", "B01001_021",
                              "B01001_022", "B01001_023", "B01001_024",
                              "B01001_025", "B01001_027", "B01001_028",
                              "B01001_029", "B01001_030", "B01001_044",
                              "B01001_045", "B01001_046", "B01001_047",
                              "B01001_048", "B01001_049"),
                geometry = TRUE)
  
  wide_acs <- acs %>% select(-moe) %>% 
    spread(variable, estimate) %>%
    rename(black = B02001_003, 
           white = B02001_002, 
           total_pop = B01003_001,
           total_hh = DP04_0001, 
           hispanic = B03003_003, 
           asian = B02001_005, 
           total_hh_poverty = C17002_001,
           hh_under_0.5_poverty = C17002_002, 
           hh_0.5_to_0.99_poverty = C17002_003,
           total_hh_car = B08201_001,
           no_cars = B08201_002,
           pct_no_car = DP04_0058P,
           pct_car_commute = S0801_C01_002,
           num_workers = S0801_C01_001,
           unemployed = B23025_005) %>%
    mutate(pct_black = black / total_pop,
           pct_white = white / total_pop,
           pct_hispanic = hispanic / total_pop,
           pct_asian = asian / total_pop,
           pct_own_car = (total_hh_car - no_cars) / total_hh_car,
           num_children = B01001_003 + B01001_004 + B01001_005 + B01001_006 +
             B01001_027 + B01001_028 + B01001_029 + B01001_030,
           num_senior = B01001_020 + B01001_021 + B01001_022 + B01001_023 +
             B01001_024 + B01001_025 + B01001_044 + B01001_045 + B01001_046 +
             B01001_047 + B01001_048 + B01001_049) %>%
    select(-starts_with("B01001"))
  
  return(wide_acs)
}


travel_time_to_closest <- function(all_data, 
                                   food_type, 
                                   dur_type,
                                   route_date) {
  
  time_to_closest <- all_data %>%
    # need to update based on data stucture
    filter({{ food_type }} > 0, date == route_date) %>%
    group_by(geoid_start) %>%
    summarise(min_duration = min({{ dur_type }}, na.rm = TRUE))
  
  return(time_to_closest)
}

map_time_to_closest <- function(county_shp, ttc, opp){

  ttc_shp <- left_join(county_shp, 
                           ttc, 
                           by = c("GEOID" = "geoid_start"))
  
  opp_formatted <- gsub("\ ", "_", tolower(opp))
  
  set_urbn_defaults(style = "map")
  urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
  
  time_to_closest <- ggplot() +
    geom_sf(data = ttc_shp, mapping = aes(fill = min_duration)) +
    scale_fill_gradientn(colours = urban_colors) +
    labs(title = str_glue("Weighted Travel Time to Closest {opp}\n in Arlington County "), 
         fill = "Time (minutes)") +
    guides(fill = guide_colourbar(barheight = 8)) 
    #theme(legend.text = element_text(size = 6) +
    ggsave(plot = time_to_closest,
           filename = here("routing/images", 
                str_glue("time_to_closest_{opp_formatted}.png")))
  
  return(time_to_closest)
}


count_accessible_within_t <- function(all_data, 
                                      food_type, 
                                      dur_type, 
                                      t, 
                                      route_date) {
  count_within_t <- all_data %>%
    filter({{ food_type }} > 0, {{ dur_type }} <= t, date == route_date) %>%
    group_by(geoid_start) %>%
    summarise(count = sum( {{ food_type }} ))
  

  return(count_within_t)
}

map_count_within_t <- function(count_within_t, county_shp){
  count_within_t <- left_join(county_shp, 
                                count_within_t, 
                                by = c("GEOID" = "start_geoid"))
  
  set_urbn_defaults(style = "map")
  urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
  
  
  count_t <- ggplot() +
    geom_sf(data = count_within_t, mapping = aes(fill = count)) +
    scale_fill_gradientn(colours = urban_colors) +
    labs(title = str_glue("Number of {food_type} accessible within {t} minutes by {mode}"), 
         fill = str_glue("Number {food_type}")) +
    guides(fill = guide_colourbar(barheight = 8)) +
    ggsave(here("images", 
                paste(food_type, "_number_within_", t, ".png", sep = "")))
  
  return(count_t)
}  
  


make_bar_plot_race <- function(acs_data, all_data, city) {
  
  avg_schools <- all_data %>%
    group_by(start) %>%
    summarise(min_duration = min(duration, na.rm = TRUE))
  
  avg_schools_demographic <- left_join(avg_schools, acs_data, by = c("start" = "GEOID")) %>%
    mutate(min_duration_black_pop = min_duration * B02001_003,
           min_duration_white_pop = min_duration * B02001_002,
           min_duration_hispanic_pop = min_duration * B03003_003,
           min_duration_asian_pop = min_duration * B02001_005)
  
  
  wt_black <- (sum(avg_schools_demographic$min_duration_black_pop, na.rm = TRUE) / 
                 sum(avg_schools_demographic$B02001_003, na.rm = TRUE))
  wt_white <- (sum(avg_schools_demographic$min_duration_white_pop, na.rm = TRUE) / 
                 sum(avg_schools_demographic$B02001_002, na.rm = TRUE))
  wt_hispanic <- (sum(avg_schools_demographic$min_duration_hispanic_pop, na.rm = TRUE) / 
                    sum(avg_schools_demographic$B03003_003, na.rm = TRUE))
  wt_asian <- (sum(avg_schools_demographic$min_duration_asian_pop, na.rm = TRUE) / 
                 sum(avg_schools_demographic$B02001_005, na.rm = TRUE))
  
  df <- data.frame(cbind(c("Black", "White", "Hispanic", "Asian"),
                         c(wt_black, wt_white, wt_hispanic, wt_asian)))
  names(df) <- c("variable", "value")
  
  set_urbn_defaults(style = "print")
  race_bar_plot <- df %>% mutate(value = round(as.numeric(as.character(value)), 2)) %>%
    ggplot(aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat="identity") +
    labs(title = paste("Weighted Average Time to Closest 2-Year College by Race in", city, sep = " "), 
         fill = "Race",
         y = "Time (minutes)",
         x = "Race") +
    geom_text(aes(label=value), vjust=-0.3, size=3.5) +
    ggsave(here("../access-analysis/images", 
                paste(city, "race_wt_avg.png", sep = "_")))
  
  return(race_bar_plot)
}

make_bar_plot_poverty_status <- function(acs_data, all_data, city) {
  avg_schools <- all_data %>%
    group_by(start) %>%
    summarise(min_duration = min(duration, na.rm = TRUE))
  
  avg_schools_demographic <- left_join(avg_schools, acs_data, by = c("start" = "GEOID")) %>%
    mutate(hh_below_pov = C17002_002 + C17002_003,
           hh_above_pov = C17002_001 - hh_below_pov,
           min_duration_below_pov = min_duration * hh_below_pov,
           min_duration_above_pov = min_duration * hh_above_pov)
  
  
  wt_below_pov <- (sum(avg_schools_demographic$min_duration_below_pov, na.rm = TRUE) / 
                     sum(avg_schools_demographic$hh_below_pov, na.rm = TRUE))
  wt_above_pov <- (sum(avg_schools_demographic$min_duration_above_pov, na.rm = TRUE) / 
                     sum(avg_schools_demographic$hh_above_pov, na.rm = TRUE))
  
  df <- data.frame(cbind(c("Above Poverty Line", "Below Poverty Line"),
                         c(wt_above_pov, wt_below_pov)))
  names(df) <- c("variable", "value")
  #df <- df %>% mutate(value = ifelse(is.infinite(value), 0, value))
  
  set_urbn_defaults(style = "print")
  pov_bar_plot <- df %>% mutate(value = round(as.numeric(as.character(value)), 2)) %>%
    ggplot(aes(x = variable, y = value, fill = variable)) +
    geom_bar(stat="identity") +
    labs(title = paste("Weighted Average Time to Closest 2-Year College by Poverty Status in", city, sep = " "), 
         fill = "Poverty Status",
         y = "Time (minutes)",
         x = "Poverty Status") +
    geom_text(aes(label=value), vjust=-0.3, size=3.5) +
    ggsave(here("../access-analysis/images", 
                paste(city, "poverty_wt_avg.png", sep = "_")),
           width = 8, height = 8)
  
  return(pov_bar_plot)
}