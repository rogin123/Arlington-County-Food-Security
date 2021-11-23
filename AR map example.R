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
urban_colors <- c("#cfe8f3", "#a2d4ec", "#73bfe2", "#46abdb", "#1696d2", "#12719e", "#0a4c6a", "#062635")


# get road shapefle
road <- roads(state = "Virginia", county = "013")

#Arlington county
ggplot() +
  geom_sf(acs_ficombo, mapping = aes(fill = FI), color = "grey") +
  #add roads to map
  geom_sf(data = road,
          color="grey", fill="white", size=0.25, alpha =.5)+
  #change food insecurity legend
  scale_fill_gradientn(colours = urban_colors, name = "Food insecurity rate", labels = percent, 
                       limits = c(0,.15) ,breaks=c(0, .05, .10, .15))+
  theme(legend.position = "right", 
        legend.box = "vertical", 
        legend.key.size = unit(1, "cm"), 
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=16))
ggsave("Final Maps/arco_fi.pdf", height = 6, width = 10, units = "in", dpi = 500, 
       device = cairo_pdf)