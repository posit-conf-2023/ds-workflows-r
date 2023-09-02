library(shiny)
library(vetiver)
library(tidyverse)

# UI --------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Query Food Inspections API"),

    # Sidebar with features for API 
    sidebarLayout(
        sidebarPanel(
            selectInput("facility_type",
                        "Facility Type:",
                        choices = c("RESTAURANT", "BAKERY", "GROCERY STORE", "COFFEE SHOP")),
            radioButtons("risk",
                         "Select Risk:",
                         choices = c(1, 2, 3)),
            numericInput("lat",
                         "Latitude:",
                         value = 41.90),
            numericInput("long",
                         "Longitude:",
                         value = -87.63),
            sliderInput("v_cumsum",
                        "Cumulative Violations:",
                        min = 0,
                        max = 100,
                        value = 20),
            sliderInput("v_cumsum_cs",
                        "Cumulative Critical/Serious\nViolations:",
                        min = 0,
                        max = 100,
                        value = 10),
            numericInput("days",
                         "Days Since Last Inspection:",
                         value = 100)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("fail"),
           tags$head(tags$style("#fail{color: blue;
                                 font-size: 50px;
                                 font-style: bold;
            }"))
        )
    )
)

# Server -----------------------------
server <- function(input, output) {
  
  # Create endpoint
  api_url <- "https://connect.conf23workflows.training.posit.co/food-inspections-api"
  
  # Add predict
  endpoint <- vetiver_endpoint(paste0(api_url, "/predict"))
  
  # Create mock data based on input
  mock_establishment <- reactive({
    tibble(facility_type = input$___,
           risk = input$___,
           latitude = input$___,
           longitude = input$___,
           v_cumsum = input$___,
           v_cumsum_cs = input$___,
           days_since_last_inspection = input$___)
  })
  
  output$fail <- renderText({
    
    # Use predict to query API
    predict(endpoint, mock_establishment())$.pred_FAIL
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
