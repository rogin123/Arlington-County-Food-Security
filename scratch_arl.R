library(ggspatial)

# load in map.R helper
source('helper_alr.R')

# scratch work for function 

map_demographic <-  function (data1 = acs_ficombo,
                              data2= foodsites2, 
                              site_display = "elig_type",
                              percent_variable = "pct_latine",
                              title1 = "Percent Latine Population", 
                              title2 = "Eligibility Type", 
                              # limit = c("No age restriction", "Open to children only", 
                              #           "Open to seniors only"),
                              colors = c("#ec008b", "#fdbf11", "#000000")
                          ){
  
  # Enquoting input for tidyverse functions 
  percent_variable <- rlang::sym(percent_variable)
  site_display <- rlang::sym(site_display)
  
  road <- roads("VA", "013")
  
  plot <- ggplot() +
    geom_sf(data=data1, aes(fill = !!percent_variable), color = "grey")+
     geom_sf(data = road,
             color="grey", fill="white", size=0.25, alpha =.5)+
    scale_fill_gradientn(name = title1, colours = urban_colors, labels = percent) +
    geom_sf(data = data2, mapping = aes(color = !!site_display), size = 1.5, show.legend = "point", inherit.aes = F) +
    scale_color_manual(name = title2, values = colors)+
                       # limits = limit)+
    theme(legend.position = "bottom")
  return(plot)
}

map_demographic(data1 = acs_ficombo,
                data2= foodsites2, 
                site_display = "type",
                percent_variable = "pct_latine",
                title1 = "Percent Latine Population", 
                title2 = "Type",
                colors = c("#ec008b", "#fdbf11", "#000000", "#1696d2"))

