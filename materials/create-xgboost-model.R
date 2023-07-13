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
# For testing purposes, I'll only read in 20,000 rows
inspections_raw <- readr::read_csv("https://data.cityofchicago.org/api/views/4ijn-s7e5/rows.csv", 
                                   n_max = 2000, 
                                   show_col_types = FALSE) |> 
  mutate(Zip = as.character(Zip))



# Clean Data ----------------------------------------------
inspections_clean <- inspections_raw |> 
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
  
  # Extract violations (v_list) and total violation numbers (v_num)
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
    Risk == "Risk 1 (High)" ~ 1,
    Risk == "Risk 2 (Medium)" ~ 2,
    Risk == "Risk 3 (Low)" ~ 3
  )) |> 
  
  # Filter for just (potential) predictors
  select(Results, `DBA Name`, `License #`, 
         `Facility Type`, Risk, Zip, Location, 
         v_cumsum, v_cumsum_cs)

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
rec <- recipe(results ~ dba_name + facility_type + risk + zip + v_cumsum + v_cumsum_cs, 
              data = inspections_train) |> 
  # Convert facility type to binary columns
  step_dummy(facility_type) |> 
  # Convert dba_name and zip (which has many levels) to a set of scores
  #  derived from a glm model that estimates the effect
  #  of each Zip code on the outcome. Likelihood encoding.
  step_lencode_glm(zip, outcome = vars(results)) |> 
  step_lencode_glm(dba_name, outcome = vars(results))

rec




# Define model specifications ----------------------------
xgb_spec <- parsnip::boost_tree(
  mode = "classification",
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = 0.01
) |> 
  set_engine("xgboost")

xgb_spec




# Create workflow ----------------------------------------
xgb_wf <- workflow() |> 
  add_recipe(rec) |> 
  add_model(xgb_spec)

xgb_wf



# Create cross-validation resamples of training set ------
set.seed(1234)
inspections_folds <- vfold_cv(inspections_train, strata = results)



# Tune Model ---------------------------------------------
registerDoParallel()

set.seed(1234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = inspections_folds,
  grid = 30,
  control = control_grid(save_pred = TRUE)
)

xgb_res




# Explore Results -----------------------------------------
collect_metrics(xgb_res)

show_best(xgb_res, "roc_auc")

xgb_best <- select_best(xgb_res, "roc_auc")



# Finalize Workflow --------------------------------------
final_xgb <- finalize_workflow(
  xgb_wf,
  xgb_best
)

final_xgb



# Fit Final Model ----------------------------------------
inspections_fit <- last_fit(final_xgb, split = inspections_split)



# Extract Final workflow ---------------------------------
inspections_wf_model <- extract_workflow(inspections_fit)



# Save/Pin Model as Vetiver Pin ------------------------------
v <- vetiver_model(
  inspections_wf_model,
  "inspections-xgboost-model"
)
v

board <- board_connect()
vetiver_pin_write(board, v)




 