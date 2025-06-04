#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(leaflet)
library(lubridate)


# ////////////////////////////////////////////////////////////////////////////
# Setup
# ////////////////////////////////////////////////////////////////////////////
saturday_bikes <- read_csv("data/saturday_bikes.csv")


# ////////////////////////////////////////////////////////////////////////////
# UI
# ////////////////////////////////////////////////////////////////////////////
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "€Capitol Bikeshare Availability - May 14, 2022"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(title = "Station Map",
        leafletOutput("map"),
        width = 12
    ),
    box("Click a station to populate",
        width = 12,
        plotOutput("plot")
    )
  )
)

# ////////////////////////////////////////////////////////////////////////////
# Server
# ////////////////////////////////////////////////////////////////////////////
server <- function(input, output, session) {
  
  # Draw station map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$map <- renderLeaflet({
    saturday_bikes %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng = median(saturday_bikes$lon),
        lat = median(saturday_bikes$lat),
        zoom = 13
      ) %>%
      addAwesomeMarkers(
        lng = ~lon,
        lat = ~lat,
        layerId = ~station_id,
        icon = awesomeIcons(
          "bicycle",
          library = "fa",
          iconColor = "white",
          markerColor = "red"
        ),
        
        label = ~paste0(name)
      )
  })
  
  
  # Get availability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  availability <- reactive({
    
    req(input$map_marker_click)
    
    saturday_bikes %>%
      filter(station_id == input$map_marker_click$id)
  })
  
  # Create plot of availability~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$plot <- renderPlot({
    
    station_name <- availability() %>%
      distinct(name) %>%
      pull()
    
    availability() %>%
      ggplot(aes(x = time, y = num_bikes_available)) +
      geom_line() +
      ggtitle(glue::glue("Bikes Available at {station_name}")) +
      xlab("Time") +
      ylab("Number of Bikes") +
      scale_x_datetime(labels = function(x) format(x - 18000, "%H:%M")) +
      scale_y_continuous(labels = round)
  })
  
}

# ////////////////////////////////////////////////////////////////////////////
# Run the app
# ////////////////////////////////////////////////////////////////////////////
shinyApp(ui = ui, server = server)