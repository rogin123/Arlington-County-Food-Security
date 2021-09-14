library(dotenv)
library(tidyverse)
library(httr)
library(here)

api_key = Sys.getenv("transit_land_api_key")


dir.create(here("routing/data/gtfs-raw"))
get_latest_feed <- function(agency_name, feed_key, api_key){
  feed_url = str_glue("https://transit.land/api/v2/rest/feeds/{feed_key}/download_latest_feed_version?api_key={api_key}")
  download.file(feed_url, 
                here("routing/data/gtfs-raw", str_glue("{agency_name}_latest.gtfs.zip")), 
                mode = "wb")
}

get_feed_date <- function(agency_name, feed_key, api_key, date){
  feed_list_url <- str_glue("https://transit.land/api/v2/rest/feeds/{feed_key}/feed_versions?api_key={api_key}")
  feed_version_key <- NULL
  while (!is.null(feed_list_url) & is.null(feed_version_key)){
    r <- GET(feed_list_url)
    
    if (status_code(r) != 200){
      Sys.sleep(61)
      r <- GET(feed_list_url)
    }
    content <- content(r)
    feed_versions <- content$feed_versions
    for (fv in feed_versions){
      if ((as.Date(fv$earliest_calendar_date) < (date - 2)) & (as.Date(fv$latest_calendar_date) > (date + 2))){
        feed_version_key <- fv$sha1
      }
      
      break 
    }
    
    if (!is.null(feed_version_key)){
      feed_url <- str_glue("https://transit.land/api/v2/rest/feed_versions/{feed_version_key}/download?api_key={api_key}")
      download.file(feed_url, 
                    here("routing/data/gtfs-raw", str_glue("{agency_name}_{date}.gtfs.zip")), 
                    mode = "wb")
    } else {
      feed_list_url <- content$meta[["next"]]
      print(feed_list_url)
      print(feed_version_key)
    }
    
  }
  
}


#Fairfax CUE
#Charles County

transit_agencies <- tribble(
  ~agency_name, ~feed_key,
"wmata_rail", "f-dqc-wmata~rail",
"bethesda_circulator", "f-bethesdacirculator~md~us",
"arlington_transit", "f-dqcjj-arlingtontransit"
)

transit_agencies <- tribble(
  ~agency_name, ~feed_key,
  "alexandria_dash", "f-dqchv-alexandriatransitcompanydash",
  "fairfax_connector", "f-dqcj-fairfaxconnector",
  "ptrc", "f-dqch-prtc",
  "loudoun_county_transit", "f-loudouncounty~md~us",
  "dc_circulator", "f-dqcjr-dccirculator", #old
  "dc_streetcar", "f-dqcm2-dcstreetcar",
  "pg_county_bus", "f-dqc-thebusofprincegeorgescounty", #old
  "montgomery_county_ride_on", "f-dqcn-montgomerycountymdrideon", 
  "wmata_bus", "f-dqc-wmata~bus",
  "wmata_rail", "f-dqc-wmata~rail",
  "bethesda_circulator", "f-bethesdacirculator~md~us",
  "arlington_transit", "f-dqcjj-arlingtontransit"
)



map2(transit_agencies$agency_name, 
     transit_agencies$feed_key, 
     get_latest_feed, 
     api_key = api_key)

map2(transit_agencies$agency_name, 
     transit_agencies$feed_key,
     get_feed_date,
     api_key = api_key,
     date = as.Date("2020-02-16"))