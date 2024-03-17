require(leaflet)
require(shiny)

shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  sidebarLayout(
    mainPanel(
      leafletOutput("city_bike_map", height = 1000)
    ),
    sidebarPanel( 
      selectInput(inputId = "city_dropdown", label = "Cities", choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris"))
    ))
))
