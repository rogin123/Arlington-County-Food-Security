library(shiny)

#load in helper document
source("scratch_arl.R")
source("helper_alr.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Food Insecurity in Arlington County"),
  sidebarLayout(
    sidebarPanel(
      ## Quick synopsis of app's purpose
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      ## drop down widget so user can select which race to create map for 
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Latine", "Percent Asian"),
                  selected = "Percent White"),
      ## drop down widget so user can select which race to create map for 
      selectInput("eligibility", 
                  label = "Select an eligibility type to display",
                  choices = c("All",
                              "No age restirction",
                              "Open to children only",
                              "Open to seniors only"),
                  selected = "All")),
  
    mainPanel(
      h1(textOutput("title"), align = "center"),
      plotOutput("map")
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  # Title for main panel 
  output$title <- renderText({
    input$var
  })
  ## Constructing and storing data into a heat map 
  output$map <- renderPlot({
    ## The plotting function percent_map()
    race_var <- switch(input$var, 
                       "Percent White" = "pct_nlwhite",
                       "Percent Black" = "pct_nlblack",
                       "Percent Latine" = "pct_latine",
                       "Percent Asian" = "pct_nlasian")
    ## Switch to identify from the UI which color should be passed into 
    ## the plotting function percent_map() - the color is tied to 
    ## which race's data is selected 
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    ## Switch to identify from the UI which legend title sshould be passed into
    ## the plotting function percent_map()
    legend <- switch(input$var, 
                     "Percent White" = "Percent White Population",
                     "Percent Black" = "Percent Black Population",
                     "Percent Hispanic" = "Percent Hispanic Population",
                     "Percent Asian" = "Percent Asian Populatin")
    
    map_demographic(data1 = wide_acs, 
                    data2 = foodsites2,
                    percent_variable = race_var,
                    title = legend)
  })
}

# Run app ----
shinyApp(ui, server)
