# Title: Create XGBoost Model
# Description: Using the Chicago Food inspections data, create an XGBoost
#    model to predict the likelihood that an establishment will fail
#    inspection

# Install Packages ----------------------------------------
install.packages("tidymodels")
install.packages("tidyverse")
install.packages("embed")
install.packages("janitor")
install.packages("vetiver")
install.packages("doParallel")
install.packages("pins")
install.packages("xgboost")

# Load Packages -------------------------------------------
library(tidymodels) # For modeling
library(tidyverse)  # For data analysis
library(embed)      # Extending recipes 
library(janitor)    # For data cleaning
library(vetiver)    # ML ops
library(doParallel) # for parallel processing
library(pins)       # Saving model to Connect

tidymodels_prefer() # for handling package conflicts



# Read in Data --------------------------------------------
# For testing purposes, I'll only read in 10,000 rows
inspections_raw <- readr::read_csv("https://data.cityofchicago.org/api/views/4ijn-s7e5/rows.csv", 
                                   n_max = 10000, 
                                   show_col_types = FALSE) |> 
  mutate(Zip = as.character(Zip))



# Clean Data ----------------------------------------------
inspections_clean <- inspections_raw |> 
  # Ensure Inspection data is dat
  mutate(`Inspection Date` = mdy(`Inspection Date`)) |>
  
  # Filter for only certain facility types
  filter(`Facility Type` %in% c("Restaurant", "Grocery Store", "School", "Bakery")) |> 
  
  # Filter for only Pass, Fail, Pass with conditions
  filter(Results %in% c("Pass", "Fail", "Pass w/ Conditions")) |>
  
  # Convert "Pass w/ Conditions" to Fail (since it is a failure, but was corrected
  #  before the food inspector left)
  mutate(Results = if_else(Results == "Pass w/ Conditions", "Fail", Results)) |> 
  
  # Filter for Chicago IL
  filter(State == "IL") |> 
  filter(City %in% c("CHICAGO")) |> 
  
  # Remove non-relevant columns
  select(-`AKA Name`, -Address) |> 
  
  # Extract violations (v_list) and total violation numbers (v_num) based 
  #  on license number
  rowwise() |>
  mutate(v_list = case_when(
    is.na(Violations) ~ NA,
    !is.na(Violations) ~ str_extract_all(Violations,
                                         "(^\\d{1,2}|\\|\\s*\\d{1,2})"))) |>
  mutate(v_list = case_when(
    is.null(v_list) ~ NA,
    !is.null(v_list) ~ list(str_replace_all(v_list, "\\|\\s*", "")))) |>
  mutate(v_num = length(v_list)) |>

  # Detect number of critical/serious violations (#'s 1 to 29)
  mutate(v_num_cs = sum(1:29 %in% unlist(v_list))) |>

  # Calculate cumsum violation number
  group_by(`License #`) |>
  arrange(`Inspection Date`) |>
  mutate(v_total = sum(v_num)) |>
  mutate(v_cumsum = cumsum(v_num)) |>
  mutate(v_cumsum_cs = cumsum(v_num_cs)) |>
  ungroup(`License #`) |>
  
  # Refactor risk
  mutate(Risk = case_when(
    Risk == "Risk 1 (High)" ~ 3,
    Risk == "Risk 2 (Medium)" ~ 2,
    Risk == "Risk 3 (Low)" ~ 1
  )) |> 
  
  # Filter for just (potential) predictors
  select(Results, `DBA Name`, `License #`, `Inspection Date`, 
         `Facility Type`, Risk, Zip, v_cumsum, v_cumsum_cs)

# Clean column names
inspections_clean <- clean_names(inspections_clean)



# Split Data --------------------------------------------
set.seed(1234)

# Define split
inspections_split <- initial_split(inspections_clean, strata = results, prop = 0.75)
inspections_split

# Create taining and testing datasets
inspections_train <- training(inspections_split)
inspections_test <- testing(inspections_split)




# Create Model Recipe -----------------------------------
rec <- recipe(results ~ facility_type + risk + zip + v_cumsum + v_cumsum_cs, 
              data = inspections_train) |> 
  # Can we just have a column to see if license number has failed before
  # Can we see if the DBA name has failed before (instead of cum sum)
  
  # Convert facility type to binary columns
  step_dummy(facility_type) |> 
  # Convert dba_name and zip (which has many levels) to a set of scores
  #  derived from a glm model that estimates the effect
  #  of each Zip code on the outcome. Likelihood encoding.
  step_lencode_glm(zip, outcome = vars(results))

rec




# Define model specifications ----------------------------
xgb_spec <- parsnip::boost_tree(
  mode = "classification",
  trees = tune(),
  min_n = tune(),
  mtry = tune(),
  learn_rate = 0.05
) |> 
  set_engine("xgboost")

xgb_spec




# Create workflow ----------------------------------------
xgb_wf <- workflow(rec, xgb_spec)

xgb_wf



# Create cross-validation resamples of training set ------
set.seed(1234)
inspections_folds <- vfold_cv(inspections_train, strata = results)



# Tune Model ---------------------------------------------
registerDoParallel()

set.seed(1234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = inspections_folds
)

xgb_res




# Explore Results -----------------------------------------
collect_metrics(xgb_res)

#collect_predictions(xgb_res) |> View()

autoplot(xgb_res)

show_best(xgb_res, "roc_auc")

xgb_best <- select_best(xgb_res, "roc_auc")



# Finalize Workflow and fit to training data -----------------------------
final_xgb <- finalize_workflow(
  xgb_wf,
  xgb_best
) |> 
  last_fit(split = inspections_split)

final_xgb

collect_metrics(final_xgb)

collect_predictions(final_xgb) |> 
  roc_curve(results, .pred_Fail) |> 
  autoplot()


# Extract Final workflow ---------------------------------
board <- board_connect()

inspections_wf_model <- extract_workflow(final_xgb) |> 
  vetiver_model("ryan/inspections-xgboost-model")


vetiver_pin_write(board, inspections_wf_model)




 