library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)

# Read in clean example data ------------------------------
inspections_clean <- read_csv("inspections_data.csv")

# Add a mock predictions column ---------------------------
inspections_results <- inspections_clean |> 
  rowwise() |> 
  mutate(.pred_Fail = runif(1, 0, 1)) |> 
  group_by(dba_name, license_number) |> 
  arrange(desc(inspection_date)) |> 
  slice(1)

# UI ------------------------------------------------------
ui <- page_sidebar(
  title = "My dashboard",
  sidebar = "Sidebar",
  "Main content area"
)


# Server --------------------------------------------------
server <- function(input, output) {}




# Run App ------------------------------------------------
shinyApp(ui, server)