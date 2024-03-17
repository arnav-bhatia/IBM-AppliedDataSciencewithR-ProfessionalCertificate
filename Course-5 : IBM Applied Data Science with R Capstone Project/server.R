require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)

source("model_prediction.R")


test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

shinyServer(function(input, output){
  
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  cities_max_bike_df = city_weather_bike_df %>% 
                        group_by(CITY_ASCII) %>%
                        arrange(desc(BIKE_PREDICTION)) %>%
                        slice(1)
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      output$city_weather_bike_df =renderLeaflet ({
        leaflet() %>% addTiles() %>% addMarkers(label=city_weather_bike_df$CITY_ASCII,lng=city_bike_map$LNG, lat=city_bike_map$LAT, 
                                                popup=city_weather_bike_df$LABEL,options = popupOptions(closeButton = FALSE))})
    }
    else {
      selected_city=reactive({ city %>% filter(CITY_ASCII==input$city_dropdown) }) 
      
      selected_city_5_day=reactive({city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)})
      
      output$city_weather_bike_df =renderLeaflet ({
        leaflet() %>% addTiles() %>% setView(lng=selected_city()$LNG, lat=selected_city()$LAT, zoom=15) %>% 
          addMarkers(lng=selected_city()$LNG, lat=selected_city()$LAT, 
                     popup=selected_city()$DETAILED_LABEL)
      })
    } 
  })
  output$city_bike_map <- renderLeaflet({
    leaflet() %>% addTiles () %>% 
    addMarkers (lng = cities_max_bike_df$LNG, lat = cities_max_bike_df$LAT,
                      popup = cities_max_bike_df$LABEL )
    
  })

  selected_city=reactive({ city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown) })

  
})
