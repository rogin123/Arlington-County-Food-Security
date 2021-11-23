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
library(gtools)


set_urbn_defaults(style = "map")
#urbnthemes :: lato_test()
#urbnthemes :: lato_import()
#extrafont::font_import(paths =c("C:/Users/FHernandez/Downloads/"), pattern = "Lato-Regular")

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

fs <- read.csv("Raw FI/Food Insecurity Rates - Arlington County.csv") %>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

fs_mfi <- read.csv("Raw FI/Food Insecurity Rates - Arlington County - MFI.csv") %>%
  mutate(tract = str_replace(str_extract(geography, "\\d+\\.?\\d+"), "\\.", ""),
         GEOID = str_pad(paste0("51013", tract), side = "right", width = 11, pad = "0")) %>%
  select(-tract)

##Merging on ACS and FI/MFI data
acs_ficombo <- wide_acs %>% left_join(combined_FI_MFI, by = "GEOID")
#acs_fs <- wide_acs %>% left_join(fs, by = "GEOID")
#acs_fsmfi <- wide_acs %>% left_join(fs_mfi, by = "GEOID")

#Retailer data
##SNAP Retailers
snap_fs <- read_csv("Final food data/Food_retailers_MAPPING.csv")
snap_fs<-snap_fs[!(snap_fs$zip_code==22306 | snap_fs$zip_code==22044),]
snap_fs <- snap_fs %>% 
  filter(location_type == "SNAP-retailer") %>% 
  select("location_name","location_type", "type", "longitude", 
         "latitude", "zip_code")

##Charitable food sites
cfs_all <- read.csv("Final food data/Food_retailers_cfs_elig.csv")

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
# foodsites1 <- snap_fs %>%
#   st_as_sf(coords = c("longitude", "latitude"),
#            crs = 4269) %>% 
#   st_transform(crs = 6487)


foodsites2 <- cfs_all %>% 
  select("location_name", "location_address", "longitude",
         "latitude", "location_type", "elig_type", "objectid","type",
         "zip_code") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4269) %>% 
  st_transform(crs = 6487)%>% 
  filter(!zip_code %in% c("22306"))%>% 
  filter(!objectid %in% c("75"))%>% 
  filter(!objectid %in% c("48"))

ggplot() +
  geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_manual(name = "Food Insecurity Rate", labels = percent, 
                       limits = factor(0,.15) ,breaks=c(0, .05, .10, .15), guide = guide_legend(reverse=TRUE)) +
  geom_sf(data = foodsites2, mapping = aes(color = elig_type),color = "#e54096",size = 1.5, show.legend = "point", inherit.aes = F) +
  scale_color_discrete(name = "Eligibility type")+
  theme(legend.position = "top")

#MISC
set_urbn_defaults(style = "map")
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")

road <- roads("VA", "013")

ggplot() +
  geom_sf(data=acs_ficombo, aes(fill = FI), color = "grey")+
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  scale_fill_gradientn(name = "Share of food insecure households", colours = urban_colors, labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15)) +
  geom_sf(data = foodsites2, mapping = aes(color = elig_type), size = 1.5, show.legend = "point", inherit.aes = F) +
  scale_color_discrete(name = NULL)+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(.75, "cm"), 
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=12)) #change legend text font size)
