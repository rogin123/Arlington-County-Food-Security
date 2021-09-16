library(opentripplanner)
library(tidyverse)
library(here)
library(parallel)
library(sf)
library(furrr)

make_config <- function(path_data){
  # Make router config with default parameters. You can edit these parameters if desired as described in 
  # (https://docs.ropensci.org/opentripplanner/articles/advanced_features.html#configuring-opentripplanner-1)
  # 
  router_config <- otp_make_config("router")
  otp_write_config(router_config,                
                   dir = path_data,
                   router = "default")
}

build_graph <- function(path_otp, path_data, memory){
  # Because this is a large road network, my computer required more than the default 2GB of memory. 
  # I have it set to 8GB which worked on my Urban laptop. This will take a couple of minutes to run.
  # 
  if(!file.exists(paste(path_data, "graphs/default/Graph.obj", sep = "/"))){
    log1 <- otp_build_graph(otp = path_otp, 
                            dir = path_data, 
                            memory = memory,
                            router = "default")
  }
}


start_otp <- function(path_otp, path_data, memory){
  # Again, set the memory to 8GB for routing. This can take a little bit of time. 
  # It will open up a browser window with the routing interface. Create OTP connection object. 
  #  
  
  log2 <- otp_setup(otp = path_otp, 
                    dir = path_data, 
                    memory = memory,
                    router = "default")
  otpcon <- otp_connect(timezone = "America/New_York",
                        router = "default")
  
  return(otpcon)
}

create_graph_setup_otp <- function(path_data, path_otp, memory){
  # set up opentripplanner for routing
  make_config(path_data)
  build_graph(path_otp, path_data, memory)
  otpcon <- start_otp(path_otp, path_data, memory)
  
  return(otpcon)
}

create_otp_plan <- function(mode, from_place, to_places, from_id, departure_time){
  # Conducts batch routing for provided transportation mode transportation mode
  
  #Identifies number of cores for batch routing
  n_cores = detectCores() - 1
  
  routes <- tryCatch(
    {routes <- otp_plan(otp_connection, 
                        fromPlace = from_place, 
                        toPlace = to_places,
                        fromID = from_id,
                        toID = to_places$tract_str_end,
                        mode = mode,
                        get_geometry = FALSE, #can change to true if want geometry of driving route
                        numItineraries = 1, #only returns single fastest itinerary
                        ncores = n_cores,
                        date_time = departure_time)},
    error = function(err){
      routes <- tibble(
        duration = NA_real_, 
        distance = NA_real_, 
        fromPlace = from_id, 
        toPlace = NA_character_
      )
      return(routes)
    }
  ) 
  
  
  if (!is.na(routes)){
    routes_out <- routes %>%
      select(duration, distance, fromPlace, toPlace) %>%
      mutate(mode = mode,
             departure_time = departure_time)
  } else {
    # if no routes can be found for a given start point, will return one row with the following:
    routes_out <- tibble(
      duration = NA_real_, 
      distance = NA_real_, 
      fromPlace = from_id, 
      toPlace = NA_character_,
      mode = mode,
      departure_time = departure_time
    )
  }
  
  
  return(routes_out)
}


batch_route <- function(df, 
                        otp_connection = otpcon, 
                        departure_time = dt){
  
  # Function to calculate a batch of driving routes between a single starting point (from_place) 
  # points to multiple destinations (to_places). Returns a dataframe with routing data including the following key fields:
  # duration (int): duration of the trip in seconds
  # distance (numeric): distance of the trip in meters
  # fromPlace (character): unique identifier for origin
  # toPlace (character): unique identifier for destination
  
  #INPUTS:
  # df (data.frame): dataframe with multiple origins and one destination.
  # otp_connection: otp connection object
  # dt (POSIX datetime): departure time of trip
  #RETURNS:
  # routes (data.frame): dataframe of route information
  
  # convert to sf dataframe for  batch routing  
  to_places <- df %>% 
    select(tract_str_end, lat_end, lon_end) %>%
    st_as_sf(coords = c("lon_end", "lat_end"),
             crs = 4326)
  
  # because df has been split on tract, can select first start_lon, start_lat, and tract
  from_lon <- df %>%
    select(lon_start) %>%
    slice(1) %>%
    pull()
  
  from_lat <- df %>%
    select(lat_start) %>%
    slice(1) %>%
    pull()
  
  from_id <- df %>%
    select(tract_str_start) %>%
    slice(1) %>%
    pull()
  
  #OTP wants coordinates as longitude, latitude
  from_place <- c(from_lon, from_lat)
  
  
  modes <- c("CAR", c("TRANSIT", "WALK"))
  all_routes <- map_dfr(modes, 
                        create_otp_plan,
                        from_place = from_place, 
                        to_places = to_places, 
                        from_id = from_id,
                        departure_time = departure_time)
  
  
  
  return(all_routes)
}

route_date <- function(df, otpcon, date){
  #date (str):Form YYYY-MM-DD hh:mm:ss, e.g. "2019-06-15 8:00:00"
  
  dt <- as.POSIXct(strptime(date, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  
  #split dataframe into a list of dataframes on county
  df_list <- split(df, f = df$tract_str_start)
  
  plan(multisession, workers = detectCores() - 1)
  # If a path cannot be found between the start and end point, the otp_plan will not return a row for that     start/end pair. Therefore, the dataframe returned by this function may be smaller than the input dataframe.
  routes <- future_map_dfr(df_list, ~batch_route(.x, otpcon, dt))
  
  return(routes)
}

calculate_routes <- function(date, df, path_otp) {
  #date (str):Form YYYY-MM-DD hh:mm:ss, e.g. "2019-06-15 8:00:00"
  
  #create folder for data and graph for given state
  osm_path <- here(str_glue("routing/otp/graphs/default"))
  dir.create(osm_path)
  
  #TODO: update to download from s3?
  #download road and transit data into folder for given state
  #download_road_transit_data(state_name, transit_path, osm_path)
  
  # Uses 8GB of memory
  memory <- 12000
  
  # set up otp by creating configuration file, building graph and starting otp
  otpcon <- create_graph_setup_otp(path_data, path_otp, memory)
  
  routes <- route_date(df, otpcon, date)
  
  otp_stop(warn = FALSE)
  
  return(routes)
}

df <- read_csv(here("routing/data", "route_pairs.csv"), 
               col_types = c("tract_str_start" = "character", 
                             "tract_str_end" = "character"))

date_list <- c("2021-09-15 8:00:00")

df_sample <- df %>% sample_frac(size = .1)

## Create folder for OTP and its Data + Download OTP ##
path_data <- here("routing/otp")
dir.create(path_data)
# downloads open trip planner program
path_otp <- otp_dl_jar(path_data, cache =  FALSE)
dir.create(here("routing/otp/graphs"))

all_routes <- map_dfr(date_list, calculate_routes, df = df_sample, path_otp = path_otp)