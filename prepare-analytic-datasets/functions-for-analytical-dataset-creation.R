library(tidyverse)

convert_to_local_datetime <- function(cleaned_data_frame, datetime_mountain_name){
  
  datetime_mountain <- cleaned_data_frame[[datetime_mountain_name]]
  datetime_pacific <- with_tz(time = cleaned_data_frame[[datetime_mountain_name]], tzone = "US/Pacific")
  datetime_central <- with_tz(time = cleaned_data_frame[[datetime_mountain_name]], tzone = "US/Central")
  datetime_eastern <- with_tz(time = cleaned_data_frame[[datetime_mountain_name]], tzone = "US/Eastern")
  
  olson <- cleaned_data_frame[["olson"]]
  
  datetime_local <- case_when(
    olson == "US/Mountain" ~ force_tz(time = datetime_mountain, tzone = "UTC"),
    olson == "US/Pacific" ~ force_tz(time = datetime_pacific, tzone = "UTC"),
    olson == "US/Central" ~ force_tz(time = datetime_central, tzone = "UTC"),
    olson == "US/Eastern" ~ force_tz(time = datetime_eastern, tzone = "UTC"),
    .default = NULL
  )
  
  return(datetime_local)
}

construct_primary_proximal_outcome <- function(cleaned_data_frame, q1_var_name, q2_var_name, q3_var_name){
  
  q1_response_value <- cleaned_data_frame[[q1_var_name]]
  q2_response_value <- cleaned_data_frame[[q2_var_name]]
  q3_response_value <- cleaned_data_frame[[q3_var_name]]
  
  primary_proximal_outcome_value <- case_when(
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="Yes" | q2_response_value=="Yes" | q3_response_value=="Yes") ~ 1,
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="No" & q2_response_value=="No" & q3_response_value=="No") ~ 0,
    !is.na(q1_response_value) & (is.na(q2_response_value) | is.na(q3_response_value)) & (q1_response_value=="Yes") ~ 1,
    !is.na(q2_response_value) & (is.na(q1_response_value) | is.na(q3_response_value)) & (q2_response_value=="Yes") ~ 1,
    !is.na(q3_response_value) & (is.na(q1_response_value) | is.na(q2_response_value)) & (q3_response_value=="Yes") ~ 1,
    .default = NULL
  )
  
  return(primary_proximal_outcome_value)
}




