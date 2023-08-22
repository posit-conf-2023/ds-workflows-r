library(shiny)
library(tidyverse)
library(leaflet)
library(pins)
library(vetiver)
library(bslib)
library(bsicons)
library(plotly)

# Read in model input data data ------------------------------
board <- board_connect()
model_inputs <- pin_read(board, "katie.masiello/inspections_model_inputs")


# Read in weekly summary data ------------------------------
inspections_per_week_this_year <- pin_read(board, "katie.masiello/inspections_per_week") |> 
  filter(`year(inspection_date)` == year(today())) |> 
  rename(week=(`week(inspection_date)`)) |> 
  mutate(pass_rate = round(100*pass_this_week/inspections_this_week),1)




# Get API info --------------------------------------------
api_url <- "https://connect.conf23workflows.training.posit.co/food-inspections-api/"
endpoint <- vetiver_endpoint(paste0(api_url, "/predict"))


# UI ------------------------------------------------------
ui <- page_fluid(
  title="City of Chicago Restaurant Inspections",
  theme = bs_theme(
    bootswatch = "solar"
  ), 
  
  layout_column_wrap(
    width = "200px",
    class = "mt-3",
    
  value_box(
    title = "Active Facilities with Inspection History",
    value = format(nrow(model_inputs),big.mark = ","),
    p(paste(format(nrow(model_inputs |> filter(facility_type == "RESTAURANT")),big.mark = ","),"Restaurants")),
    p(paste(format(nrow(model_inputs |> filter(facility_type == "GROCERY STORE")),big.mark = ","),"Grocery Stores")),
    p(paste(format(nrow(model_inputs |> filter(facility_type == "BAKERY")),big.mark = ","),"Bakeries")),
    p(paste(format(nrow(model_inputs |> filter(facility_type == "COFFEE SHOP")),big.mark = ","),"Coffee Shops")),
    p(paste(format(nrow(model_inputs |> filter(facility_type == "WRIGLEY FIELD ROOFTOP")),big.mark = ","),"Wrigley Field Rooftop")),
    
    showcase = bs_icon("card-checklist"), 
    theme_color = "primary"
  ), 
  value_box(
    title = "Inspections this week",
    value = tail(inspections_per_week_this_year$inspections_this_week,1),
    p(paste("Fewest per week", min(inspections_per_week_this_year$inspections_this_week))),
    p(paste("Averaging", round(mean(inspections_per_week_this_year$inspections_this_week),0), "per week")),
    p(paste("Most per week", max(inspections_per_week_this_year$inspections_this_week))),
    showcase = plotlyOutput("weekly_inspections"),
    full_screen = TRUE,
    theme_color = "secondary"
  ),
  value_box(
    title = "Inspection Pass Rate",
    value = paste0(tail(inspections_per_week_this_year$pass_rate,1),"%"),
    p(paste0("Worst weekly rate ", min(inspections_per_week_this_year$pass_rate),"%")),
    p(paste0("Averaging ", round(mean(inspections_per_week_this_year$pass_rate),0), "% per week")),
    p(paste0("Most per week ", max(inspections_per_week_this_year$pass_rate),"%")),
    showcase = plotlyOutput("weekly_pass_rate"),
    full_screen = TRUE,
    theme_color = "success"
  )
  ),
  br(),
  leafletOutput("map"),
  tableOutput("business_info")
)


# Server --------------------------------------------------
server <- function(input, output) {
  
  
  output$weekly_inspections <- renderPlotly({
    plotly_time_series(
      inspections_per_week_this_year, x = ~week, y = ~inspections_this_week
    )
  })
  
  output$weekly_pass_rate <- renderPlotly({
    plotly_time_series(
      inspections_per_week_this_year, x = ~week, y = ~pass_rate
    )
  })
  
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
  
  
  # From https://github.com/rstudio/bslib/blob/main/inst/examples/value_box/app.R
  plotly_time_series <- function(d, x, y) {
    info <- getCurrentOutputInfo()
    large <- isTRUE(info$height() > 200)
    
    plot_ly(d, x = x, y = y) %>%
      add_lines(
        color = I(info$fg()),
        span = I(1),
        #hoverinfo = if (!large) "none",
        fill = 'tozeroy',
        alpha = 0.2
      ) %>%
      layout(
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = info$fg()),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        xaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        ),
        yaxis = list(
          title = "",
          visible = large,
          showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  
}


# Run App ------------------------------------------------
shinyApp(ui, server)