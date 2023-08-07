library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(pins)

# Read in model input data data ------------------------------
board <- board_connect()
model_inputs <- pin_read(board, "katie.masiello/inspections_model_inputs")

# Get API info --------------------------------------------
api_url <- "https://colorado.posit.co/rsc/content/74a10e63-87de-47fc-879a-adca0903ad57"
endpoint <- vetiver_endpoint(paste0(api_url, "/predict"))


# UI ------------------------------------------------------
ui <- fluidPage(
  leafletOutput("map"),
  tableOutput("business_info")
)


# Server --------------------------------------------------
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    model_inputs |> 
      leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |>  
      setView(
        # Set to Hyatt Regency in Chicago
        lng = -87.6217,
        lat = 41.8874,
        zoom = 16
      ) |> 
      addAwesomeMarkers(
        clusterOptions = markerClusterOptions(),
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~license_number,
        icon = awesomeIcons(
          "building",
          library = "fa",
          markerColor = "red"
        ),
        
        label = ~paste0(aka_name)
      )
  })
  
  # Get selected point info
  selected_info <- reactive({
    
    req(input$map_marker_click)
    
    model_inputs |> 
      filter(license_number == input$map_marker_click$id) |> 
      select(aka_name, facility_type, risk, latitude, 
             longitude, v_cumsum, v_cumsum_cs, days_since_last_inspection) |> 
      # convert days to number
      mutate(days_since_last_inspection = 
               as.numeric(gsub(" days", "", days_since_last_inspection)))
  })
  
  # Make prediction (add .pred_FAIL to table)
  prediction_info <- reactive({
    
    prediction_df <- selected_info() |> 
      select(-aka_name) 
    
    fail_prediction <- predict(endpoint, prediction_df)$.pred_FAIL
    
    selected_info() |> 
      mutate(fail_prediction = round(fail_prediction * 100, 3))
      
  })
  
  
  output$business_info <- renderTable({
    prediction_info()
  })
}

  






# Run App ------------------------------------------------
shinyApp(ui, server)