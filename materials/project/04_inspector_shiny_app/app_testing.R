library(tidyverse)
library(leaflet)
library(pins)
library(vetiver)

# Read in model input data data ------------------------------
board <- board_connect()
model_inputs <- pin_read(board, "katie.masiello/inspections_model_inputs")

# Example input/prediction to model -------------------------------
api_url <- "https://colorado.posit.co/rsc/content/74a10e63-87de-47fc-879a-adca0903ad57"
endpoint <- vetiver_endpoint(paste0(api_url, "/predict"))

example_input <- tibble(
  facility_type = "RESTAURANT",
  risk = 3,
  latitude = 41.79799,
  longitude = -87.74830,
  v_cumsum = 20,
  v_cumsum_cs = 7,
  days_since_last_inspection = 150
)

example_input1 <- model_inputs |> 
  # pick a random license number
  filter(license_number == 11735) |> 
  select(facility_type, risk, latitude, longitude, v_cumsum, v_cumsum_cs, days_since_last_inspection) |> 
  # convert days to number
  mutate(days_since_last_inspection = as.numeric(gsub(" days", "", days_since_last_inspection)))
  

# Make a prediction using R code
test_prediction <- predict(endpoint, example_input1)$.pred_FAIL

