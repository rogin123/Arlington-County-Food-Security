# Date: November 24, 2020 
# Project: ArCo Food insecurity
# Author: Fernando Hernandez
# Task: Map out race, poverty, and food insecurity
# Skip the following steps if urbnmapr and associated packages already installed:
  #install.packages("tidyverse")
  #**ggplot2 included within tidyverse**
  #install.packages("devtools")
  #install.packages("tidycensus")                           
  #install.packages("patchwork")
#devtools::
remotes::install_github("UrbanInstitute/urbnthemes")
remotes::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(ggplot2)
library(patchwork)
library(urbnthemes)
library(urbnmapr)
library (tigris)
library(sf)
library(dplyr) 
library(censusapi)
library(tidycensus)  
library(forcats)
library(gridExtra)
library(haven)

set_urbn_defaults(style = "map")
urbnthemes :: lato_test()
urbnthemes :: lato_install()
extrafont::font_import(paths =c("C:/Users/FHernandez/Downloads/"), pattern = "Lato-Regular")

setwd("C:/Users/FHernandez/Desktop/Arlington County Food Security/Quantitative Analysis/")


  ## Uses tidycensus::get_acs function to query API and obtain ACS estimates
  ## for defined variables. Reshapes data frame to wide.
  census_api_key('3a2ea8d29baee033d48931425c57035f6ed1f2f7', overwrite = TRUE, install = TRUE)
  
  acs = get_acs(state = "51", county = "013", geography = "tract", 
                variables = c("B03002_001", "B03002_002", "B03002_003",
                              "B03002_004", "B03002_006", "B03002_012",
                              "B03002_013", "B03002_014", "B03002_016",
                              "DP04_0001", "B19013_001", "C17002_001", 
                              "C17002_002", "C17002_003", "C17002_004", 
                              "C17002_005", "C17002_006", "B25064_001", 
                              "B25070_001", "B25070_008", "B25070_009", 
                              "B25070_010", "B28002_001", "B28002_002"),
                geometry = TRUE)
  
  wide_acs <- acs %>% select(-moe) %>% 
    spread(variable, estimate) %>%
    rename(total_pop = B03002_001,
           nonlatine = B03002_002,
           nlwhite = B03002_003,
           nlblack = B03002_004, 
           nlasian = B03002_006, 
           latine = B03002_012,
           lwhite = B03002_013,
           lblack = B03002_014, 
           lasian = B03002_016, 
           total_hh = DP04_0001, 
           medhhinc = B19013_001,
           total_hh_poverty = C17002_001,
           hh_under_0.5_poverty = C17002_002, 
           hh_0.5_to_0.99_poverty = C17002_003,
           hh_1_to_1.24_poverty = C17002_004,
           hh_1.25_to_1.49_poverty = C17002_005,
           hh_1.5_to_1.84_poverty = C17002_006,
           medrent = B25064_001,
           total_pctincrent = B25070_001,
           pctincrent35_to_39.9 = B25070_008,
           pctincrent40_to_49.9 = B25070_009,
           pctincrent50_ormore = B25070_010,
           total_ipop = B28002_001,
           hh_inet = B28002_002) %>%
    mutate(pct_nlblack = nlblack / total_pop,
           pct_nlwhite = nlwhite / total_pop,
           pct_nlasian = nlasian / total_pop,
           pct_latine = latine / total_pop,
           pct_lblack = lblack / total_pop,
           pct_lwhite = lwhite / total_pop,
           pct_lasian = lasian / total_pop,
           pct_185pov = (hh_under_0.5_poverty + hh_1_to_1.24_poverty + hh_1.25_to_1.49_poverty  + hh_1.5_to_1.84_poverty)/total_hh_poverty,
           pct_incrent35 = (pctincrent35_to_39.9 + pctincrent40_to_49.9 + pctincrent50_ormore)/total_pctincrent,
           pct_inetaccess = hh_inet/total_ipop,
           comcolor <- ifelse(pct_nlwhite <0.40, 1, 0))
  
  arco_tracts <- tigris::tracts(state = "VA",
                              cb = TRUE,
                              class = "sf") 
  arco_tracts <- subset(arco_tracts, COUNTYFP == "013")
  
  snap_fs <- read.csv("https://raw.githubusercontent.com/fhernandez-urban/Arlington-County-Food-Security/routing/Retail%20data/Food_resources.csv")
  snap_fs<-snap_fs[!(snap_fs$zip_code==22306 | snap_fs$zip_code==22044),]
  
  foodsites <- snap_fs %>%
    st_as_sf(coords = c("Longitude", "Latitude"),
    crs = 4269) %>% 
    st_transform(crs = 6487)
  
set_urbn_defaults(style = "map")
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")

##putting them all in one (pending)
  ## PCT white
plot1 <-wide_acs %>% 
  ggplot() +
  geom_sf(data=, aes(fill = pct_nlwhite))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
    ##Plot 1 is my attempt at overlaying the lat/long food sites onto the map.\
    ##Error: Insufficient values in manual scale. 1 needed but only 0 provided.

## PCT Black
plot2 <-wide_acs %>% 
  ggplot() +
  geom_sf(data=, aes(fill = pct_nlblack))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)

## PCT Asian
plot3 <-wide_acs %>% 
  ggplot() +
  geom_sf(data=, aes(fill = pct_nlasian))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F) 

## PCT Latine
plot4 <-wide_acs %>% 
  ggplot() +
  geom_sf(data=, aes(fill = pct_latine))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
    

(plot1 + plot2) / (plot3 + plot4)
  ##Error: stat_sf requires the following missing aesthetics: geometry

##putting them in separate files
  ## PCT white
  ggplot() +
  geom_sf(data=wide_acs, aes(fill = pct_nlwhite))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = 1, show.legend = "point", inherit.aes = F)
  ggsave("Final Maps/pct_nlwhite.png", height = 6, width = 12, units = "in", dpi = 500)
  
  ## PCT Black
  ggplot() +
  geom_sf(data=wide_acs, aes(fill = pct_nlblack))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
  ggsave("Final Maps/pct_nlblack.png", height = 6, width = 12, units = "in", dpi = 500)

  ## PCT Asian
  ggplot() +
  geom_sf(data=wide_acs, aes(fill = pct_nlasian))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F) 
  ggsave("Final Maps/pct_nlasian.png", height = 6, width = 12, units = "in", dpi = 500)
  
  ## PCT Latine
  ggplot() +
  geom_sf(data=wide_acs, aes(fill = pct_latine))+
  scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
  geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
  ggsave("Final Maps/pct_latine.png", height = 6, width = 12, units = "in", dpi = 500)
  
  ## Median income
  ggplot() +
    geom_sf(data=wide_acs, aes(fill = medhhinc))+
    scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
    geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
  ggsave("Final Maps/pct_medhhinc.png", height = 6, width = 12, units = "in", dpi = 500)
  
  ##185 pov rate
  ggplot() +
    geom_sf(data=wide_acs, aes(fill = pct_185pov))+
    scale_fill_gradientn(name = "Percent White population", colours = urban_colors) +
    geom_sf(data = foodsites, mapping = aes(color =type), size = .4, show.legend = "point", inherit.aes = F)
  ggsave("Final Maps/pct_185pov.png", height = 6, width = 12, units = "in", dpi = 500)
