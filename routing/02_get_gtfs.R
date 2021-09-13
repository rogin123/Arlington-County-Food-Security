library(dotenv)
library(tidyverse)

api_key = Sys.getenv("transit_land_api_key")


dir.create(here("routing/data/gtfs-raw"))
get_latest_feed <- function(agency_name, feed_key, api_key){
  feed_url = str_glue("https://transit.land/api/v2/rest/feeds/{feed_key}/download_latest_feed_version?api_key={api_key}")
  download.file(feed_url, 
                here("routing/data/gtfs-raw", str_glue("{agency_name}.gtfs.zip")), 
                mode = "wb")
}


#Fairfax CUE
#Charles County

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

map2(transit_agencies$agency_name, transit_agencies$feed_key, get_latest_feed, api_key = api_key)