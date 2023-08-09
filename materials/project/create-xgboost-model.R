# Title: Create XGBoost Model
# Description: Using the Chicago Food inspections data, create an XGBoost
#    model to predict the likelihood that an establishment will fail
#    inspection

# Load Packages -------------------------------------------
library(tidymodels) # For modeling
library(tidyverse)  # For data analysis
library(embed)      # Extending recipes 
library(janitor)    # For data cleaning
library(vetiver)    # ML ops
library(doParallel) # for parallel processing
library(pins)       # Saving model to Connect
library(plumber)    # For serving model as an API
library(arrow)      # for reading pinned data

tidymodels_prefer() # for handling package conflicts



# Read in Data (should be from database, not pin) ------------------
board <- board_connect()
inspections_data <- pin_read(board, "katie.masiello/inspections_processed")


# Split Data --------------------------------------------
set.seed(1234)

# Define split
inspections_split <- initial_split(inspections_data, strata = results, prop = 0.75)
inspections_split

# Create training and testing datasets
inspections_train <- training(inspections_split)
inspections_test <- testing(inspections_split)



# Create Model Recipe -----------------------------------
rec <- recipe(results ~ facility_type + risk + latitude + longitude + 
                v_cumsum + v_cumsum_cs + days_since_last_inspection, 
              data = inspections_train) |> 

  
  # Convert facility type to binary columns
  step_dummy(facility_type)
  # Convert dba_name and zip (which has many levels) to a set of scores
  #  derived from a glm model that estimates the effect
  #  of each Zip code on the outcome. Likelihood encoding.
  #   step_lencode_glm(zip, outcome = vars(results))

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
  roc_curve(results, .pred_FAIL) |> 
  autoplot()


# Extract Final workflow ---------------------------------
inspections_wf_model <- extract_workflow(final_xgb) |>
  vetiver_model("ryan/inspections-xgboost-model")


vetiver_pin_write(board, inspections_wf_model)

# Serve the plumber API --------------------------------
vetiver_deploy_rsconnect(
  board = board, 
  name = "ryan/inspections-xgboost-model",
  predict_args = list(debug = TRUE, type = "prob"),
  account = "ryan",
  appName = "inspections-predict-api"
)


 