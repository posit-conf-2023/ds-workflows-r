library(RSocrata)
library(tidyverse)

############### Violation count function ########################
# By inspection ID, this function will extract the violation    #
# numbers (v_list), total number of violations (v_num), number  #
# of critical/serious violations (v_num_cs), and total &        #
# cumulative number of violations over time (v_total, v_cumsum) #
#################################################################

count_violations <- function() {
  
  # Read in data and select relevant columns
  message("Reading in food inspection data")
  temp_fi <- read.socrata(url = "https://data.cityofchicago.org/resource/4ijn-s7e5.json") |> 
    as_tibble() |> 
    select(-aka_name, -address, -location.latitude, -location.longitude)
  
  # Extract violations and total violation numbers
  message("Calculating number of violations")
  temp_fi_v <- temp_fi |> 
    rowwise() |> 
    mutate(v_list = case_when(
      is.na(violations) ~ NA,
      !is.na(violations) ~ str_extract_all(violations, 
                                           "(^\\d{1,2}|\\|\\s*\\d{1,2})"))) |> 
    mutate(v_list = case_when(
      is.null(v_list) ~ NA,
      !is.null(v_list) ~ list(str_replace_all(v_list, "\\|\\s*", "")))) |> 
    mutate(v_num = length(v_list))
  
  # Detect number of critical/serious violations (#'s 1 to 29)
  message("Calculating number of critical/serious violations")
  temp_fi_v_cs <- temp_fi_v |> 
    mutate(v_num_cs = sum(1:29 %in% unlist(v_list)))
  
  # Calculate cumsum violation number
  message("Calculating total/cumulative number of violations")
  temp_fi_v_cs_cumsum <- temp_fi_v_cs |> 
    group_by(license_) |> 
    arrange(inspection_date) |> 
    mutate(v_total = sum(v_num)) |> 
    mutate(v_cumsum = cumsum(v_num)) |> 
    mutate(v_cumsum_cs = cumsum(v_num_cs))
  
  # Return final dataframe
  return(temp_fi_v_cs_cumsum)
  
  # Clean
  rm(temp_fi, temp_fi_v, temp_fi_v_cs, temp_fi_v_cs_cumsum)
}

#!#!#!#!#! Testing Area #!#!#!#!#!#!
#foo <- count_violations()
