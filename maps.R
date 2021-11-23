# Date: November 24, 2020 
# Project: ArCo Food insecurity
# Author: Fernando Hernandez
# Task: Map out race, poverty, and food insecurity
# Skip the following steps if urbnmapr and associated packages already installed:
#install.packages("tidyverse")
#*ggplot2 included within tidyverse**
#install.packages("devtools")
#install.packages("tidycensus")                           
#install.packages("patchwork")
#devtools::
remotes::install_github("UrbanInstitute/urbnthemes")
remotes::install_github("UrbanInstitute/urbnmapr")
library(Cairo)
library(tidyverse)
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
library(scales)
library(readxl)
library(extrafont)

set_urbn_defaults(style = "map")
urbnthemes :: lato_import()
urbnthemes :: lato_test()
extrafont::font_import(paths =c("C:/Users/Arogin/Downloads/Lato"), pattern = "Lato-Regular", prompt = FALSE)

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
                            "B25070_010", "B28002_001", "B28002_002",
                            "B05001_001", "B05001_005", "B05001_006"),
              geometry = T)

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
         hh_inet = B28002_002,
         tpop_nat = B05001_001,
         nat_nat = B05001_005,
         nat_noncit = B05001_006) %>%
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
         comcolor <- ifelse(pct_nlwhite <0.40, 1, 0),
         pct_nonnat = (nat_nat + nat_noncit)/tpop_nat,
         pct_noncit = (nat_noncit)/tpop_nat)

#write.csv(wide_acs, "wide_acs.csv", row.names = F)

arco_tracts <- tigris::tracts(state = "VA",
                              cb = TRUE,
                              class = "sf") 
arco_tracts <- subset(arco_tracts, COUNTYFP == "013")

#FI/MFI data
combined_FI_MFI <- read_excel("Raw FI/Combined FI-MFI.xlsx")%>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

##Merging on ACS and FI/MFI data
acs_ficombo <- wide_acs %>% left_join(combined_FI_MFI, by = "GEOID")

#Retailer data
##SNAP Retailers
snap_fs <- read_csv("Final food data/Food_retailers_MAPPING.csv")
snap_fs<-snap_fs[!(snap_fs$zip_code==22306 | snap_fs$zip_code==22044),]
snap_fs <- snap_fs %>% mutate(freq_flag = case_when(frequency %in% c(
                                "3 times weekly", "3 times weekly. Participants can come once a week.", 
                                "Daily", "Open Mon-Sat. Participants can visit once per week.", "Twice weekly", 
                                "Weekly") ~ 1, 
                                TRUE ~ 0)) %>% 
  relocate(freq_flag, .after = "frequency")

##Charitable food sites
cfs_all <- read.csv("Final food data/Food_retailers_cfs_o2a.csv")
cfs_kids <- read.csv("Final food data/Food_retailers_cfs_child.csv")
cfs_elder <- read.csv("Final food data/Food_retailers_cfs_elder.csv")

##Non-SNAP retailers
non_snap <-read.csv("non_snap-geocoded.csv") %>% 
  filter(!zip %in% c(22302, 22041, 22044, 22305)) %>% 
  select(c("location_name","location_type", "Longitude", 
           "Latitude"))  %>% 
  rename(type = location_type, 
         longitude = Longitude, 
         latitude = Latitude) %>% 
  mutate(location_type = "Non-SNAP retailer") %>% 
  relocate(location_type, .before = type)

snap <- rbind(snap_fs, non_snap) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487) 

##Setting geo

# all food sites
fsite_all <- snap_fs %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)

# Just snap food sites
fsite_snap <- snap_fs %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!location_type %in% c("Charitable food-site"))%>% 
  filter(!location_type %in% c("School summer feeding sites"))

# Just charitable food sites
fs_cfsall <- fsite_all %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487) %>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75", "48"))%>% 
  filter(!location_type %in% c("SNAP-retailer"))%>% 
  filter(!location_type %in% c("School summer feeding sites"))


charitable_fullaccess <- fs_cfsall %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487) %>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75", "48"))%>% 
  filter(year_round_or_dates == "Year-round",
        elig_type == "No age restriction",
        freq_flag == 1)

charitable_flexible <- charitable_fullaccess %>% 
  filter(day %in% c("Saturday", "Sunday", "Mon-Fri, Sat"))

fs_cfsall <- fsite_all %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75"))%>% 
  filter(!objectid %in% c("48"))%>% 
  filter(!location_type %in% c("SNAP-retailer"))%>% 
  filter(!location_type %in% c("School summer feeding sites"))



#MISC
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")
fsite_colors <- c("#ec008b", "#fdbf11", "#000000")
cfs1_colors <- c("#db2b27", "#fdbf11")
two_color <- c("#ec008b", "#fdbf11")
three_color <- c("#ec008b", "#fdbf11", "#000000")
four_color <- c("#55b748", "#db2b27", "#696969", "#fdbf11")
two_color2 <- c("#55b748", "#fdbf11")

# Function to produce maps ------------------------------------------------

# get road shapefle
road <- roads(state = "Virginia", county = "013")


#function to make demographic map
##All retailers (except nonSNAP)
map_all <-  function (data1 = acs_ficombo,data2=fsite_all, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#e54096",size = 2.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
}

##SNAP retailers
map_snap <-  function (data1 = acs_ficombo,data2=fsite_snap, percent_variable = "pct_latine", title = "Percent Latine Population"){
  percent_variable <- rlang::sym(percent_variable)
  plot <- ggplot() +
    geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = "Food Insecurity Rate", colours = urban_colors, labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
    geom_sf(data = fsite_all, mapping = aes(color = elig_type),color = "#fdbf11",
            size = 2.5, show.legend = "point", inherit.aes = F) +
    scale_color_discrete(name = "Eligibility type")+
    theme(legend.position = "left")
  return(plot)
}

# MAPS
#Arlington county
ggplot() +
  geom_sf(acs_ficombo, mapping = aes(fill = FI), color = "grey") +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16))
ggsave("Final Maps/arco_fi.pdf", height = 6, width = 10, units = "in", dpi = 500, 
       device = cairo_pdf)

#FI and all food sites
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fsite_all, mapping = aes(color = location_type, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Type of food site", values = fsite_colors)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_fi.png", height = 6, width = 12, units = "in", dpi = 500)

#Food insecurity and SNAP retailers
map_snap(percent_variable = "FI", title ="Food insecurity rate")
ggsave("Final Maps/snap_fi.png", height = 6, width = 12, units = "in", dpi = 500)

###########

#Food security and access to CFS

#CFS OPEN YR AND DURING WEEKENDS AND NO ELIGIBILITY REQ
ggplot() +
  geom_sf(acs_ficombo,mapping = aes(fill = FI), color = "gray") +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  geom_sf(data = charitable_fullaccess, mapping = aes(color = location_type),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = NULL, values = "#fdbf11")+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16)) #change legend text font size)
  ggsave("Final Maps/fsites_cfs_fullaccess.pdf", height = 6, width = 10, units = "in", dpi = 300,
         device = cairo_pdf)

  #CFS OPEN YR AND DURING WEEKENDS AND NTH
  ggplot() +
    geom_sf(acs_ficombo,mapping = aes(fill = FI), color = "gray") +
    geom_sf(data = road,
            color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                         limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
    geom_sf(data = charitable_flexible,mapping = aes(color = location_type),size = 2.5, 
            show.legend = "point", inherit.aes = F) +
    scale_color_manual(name = NULL, values = "#fdbf11")+
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.key.size = unit(1, "cm"), 
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=16)) #change legend text font size)  
  ggsave("Final Maps/cfs_flexibleaccess.pdf", height = 6, width = 10, units = "in", dpi = 500, 
         device = cairo_pdf)
  
  
#ALL CFS 
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fs_cfsall, mapping = aes(color = charitablefs, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Charitable food sites", values = "#db2b27")+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_cfs_only.png", height = 6, width = 12, units = "in", dpi = 500)


#ALL CFS BY WHO HAS ACCESS
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fs_cfsall, mapping = aes(color = access, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Open year-round, during weekends and NTH", values = four_color)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_cfs_access_conditions.png", height = 6, width = 12, units = "in", dpi = 500)

#Food sites open yearly
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fs_cfsyround, mapping = aes(color = year_round, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Open year-round", values = two_color)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_cfs_yearround.png", height = 6, width = 12, units = "in", dpi = 500)  

#Food sites by frequency
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fs_cfsfreq, mapping = aes(color = frequency_visit, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Frequency of visit", values = two_color)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_cfs_frequency.png", height = 6, width = 12, units = "in", dpi = 500)

#Food sites open NTH
ggplot(acs_ficombo, aes(fill = FI)) +
  geom_sf() +
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "left")+
  geom_sf(data = fs_cfsnth, mapping = aes(color = open_nth, ),size = 2.5, 
          show.legend = "point", inherit.aes = F) +
  scale_color_manual(name = "Open during NTH", values = two_color2)+
  theme(legend.position = "right")
ggsave("Final Maps/fsites_cfs_nth.png", height = 6, width = 12, units = "in", dpi = 500)