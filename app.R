library(shiny)
library(Rcpp)
library(httpuv)
library(plotly)
#devtools::install_github('rstudio/httpuv')
#install.packages("shiny", version='1.6')

#load in helper document
source("scratch_arl.R")
source("helper_alr.R")

#options(shiny.minified=FALSE)

# User interface ----
ui <- fluidPage(
  titlePanel("Food Insecurity in Arlington County"),
  sidebarLayout(
    sidebarPanel(
      ## Quick synopsis of app's purpose
      helpText("Create maps of Arlington County Food Security"),
      ## drop down widget so user can select which race to create map for 
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Latine", "Percent Asian",
                              "Food Insecurity", "Marginal Food Insecurity"),
                  selected = "Percent White"),
      # radio button to determine which food site type/options to display
      shinyjs::useShinyjs(),
      radioButtons("sites",
                   label = "Select which type of food site to display",
                   choices = c("Charitable food sites","Retailers"),
                   selected = "Charitable food sites",
                   inline = TRUE,
                   width = "1000px"),
    conditionalPanel(
      condition = "input.sites == 'Charitable food sites'",
      ## checkbox widget so user can select which criteria to visualize
      checkboxGroupInput("eligibility",
                         label = "Select an eligibility type to display",
                         choices = c("No age restriction",
                                     "Open to children only",
                                     "Open to seniors only"),
                         selected = "No age restriction"),),
    conditionalPanel(
      condition = "input.sites == 'Retailers'",
      checkboxGroupInput("retail",
                       label = "Select a retailer type to display",
                       choices = c("SNAP retailer" = "SNAP-retailer",
                                   "Non-SNAP retailer" = "Non-SNAP retailer"),
                       selected = "Non-SNAP retailer"),),
  ),

    mainPanel(
      h1(textOutput("title"), align = "center"),
      plotOutput("map")
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  
  # Title for main panel 
  output$title <- renderText({
    input$sites
  })
  ## Constructing and storing data into a heat map 
  output$map <- renderPlot({
    # The plotting function percent_map()
    measure <- switch(input$sites,
                      "Charitable food sites" = "elig_type",
                      "Retailers" = "location_type")

    if(input$sites == "Charitable food sites"){
      eligibility_var <- foodsites2 %>% 
        filter(elig_type %in% c(input$eligibility))
      
      limit <- c("No age restriction", "Open to children only", 
                 "Open to seniors only")
      colors <- c("#ec008b", "#fdbf11", "#000000")
    }
    if(input$sites == "Retailers"){
      eligibility_var <- snap %>% 
        filter(location_type %in% c(input$retail))
      
      limit <- c("SNAP retailers", "Non-SNAP retailers")
      colors <- c("#ec008b", "#fdbf11")
    }

    race_var <- switch(input$var,
                       "Percent White" = "pct_nlwhite",
                       "Percent Black" = "pct_nlblack",
                       "Percent Latine" = "pct_latine",
                       "Percent Asian" = "pct_nlasian",
                       "Food Insecurity" = "FI",
                       "Marginal Food Insecurity" = "MFI")
    ## Switch to identify from the UI which legend title should be passed into
    ## the plotting function percent_map()
    legend <- switch(input$var,
                     "Percent White" = "% White Population",
                     "Percent Black" = "% Black Population",
                     "Percent Latine" = "% Latine",
                     "Percent Hispanic" = "% Hispanic Population",
                     "Percent Asian" = "% Asian Population",
                     "Food Insecurity" = "% Food Insecurity",
                     "Marginal Food Insecurity" = "% Marginal Food Insecurity")

    map_demographic(data1 = acs_ficombo,
                    data2 = eligibility_var,
                    site_display = measure,
                    percent_variable = race_var,
                    title1 = legend,
                    limit = limit, 
                    colors = colors)
  })
}

# Run app ----
shinyApp(ui = ui, server = server)
