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

count_total_items_with_response_ema <- function(cleaned_data_frame){
  
  tmp <- cleaned_data_frame %>% select(all_of(ends_with("_response")))
  tmp <- !is.na(tmp)
  total_ema_items_with_response <- apply(tmp, 1, sum)
  cleaned_data_frame[["total_ema_items_with_response"]] <- total_ema_items_with_response
  
  return(cleaned_data_frame[["total_ema_items_with_response"]])
}

collapse_survey_ema_status <- function(cleaned_data_frame){
  
  cleaned_data_frame[["total_ema_items_with_response"]] <- NA_real_
  cleaned_data_frame[["total_ema_items_with_response"]] <- count_total_items_with_response_ema(cleaned_data_frame)
  
  cleaned_data_frame <- cleaned_data_frame %>%
    mutate(status_survey_ema_collapsed = case_when(
      (!is.na(ts_ema_triggered_mountain)) & (total_ema_items_with_response > 0) & (status_survey_ema == "completed") ~ "fully_completed",
      (!is.na(ts_ema_triggered_mountain)) & (total_ema_items_with_response > 0) & (status_survey_ema != "completed") ~ "partially_completed",    
      (!is.na(ts_ema_triggered_mountain)) & (total_ema_items_with_response == 0) & (status_survey_ema != "completed") ~ "no_response_but_triggered",
      is.na(ts_ema_triggered_mountain) ~ "no_response_and_not_triggered",
      .default = NULL
    ))
  
  return(cleaned_data_frame[["status_survey_ema_collapsed"]])
}

count_total_items_with_response_2qs <- function(cleaned_data_frame){
  
  cleaned_data_frame <- cleaned_data_frame %>%
    mutate(is_missing_cig_avail = if_else(is.na(cig_available), 1, 0),
           is_missing_neg_affect = if_else(is.na(negative_affect), 1, 0)) %>%
    mutate(total_2qs_items_with_response = abs(1-is_missing_cig_avail) + abs(1-is_missing_neg_affect))
  
  return(cleaned_data_frame[["total_2qs_items_with_response"]])
}

collapse_survey_2qs_status <- function(cleaned_data_frame){
  
  cleaned_data_frame[["total_2qs_items_with_response"]] <- NA_real_
  cleaned_data_frame[["total_2qs_items_with_response"]] <- count_total_items_with_response_2qs(cleaned_data_frame)
  
  cleaned_data_frame <- cleaned_data_frame %>%
    mutate(status_survey_2qs_collapsed = case_when(
      total_2qs_items_with_response == 2 ~ "fully_completed",
      total_2qs_items_with_response == 1 ~ "partially_completed",  # Turns out, there are no 2qs that fall into the partially completed category; either they are fully completed or no response to any item was provided.
      (total_2qs_items_with_response == 0) & (!is.na(ts_2qs_triggered_mountain)) ~ "no_response_but_triggered",
      (total_2qs_items_with_response == 0) & (is.na(ts_2qs_triggered_mountain)) ~ "no_response_and_not_triggered",
      .default = NULL
    ))
  
  return(cleaned_data_frame[["status_survey_2qs_collapsed"]])
}

clean_response_substance_use_multiple_select <- function(cleaned_data_frame){

  # Note that cleaned_data_frame must have a column named Q19_response
  
  cleaned_data_frame <- cleaned_data_frame %>%
    mutate(Q19_response_cleaned_none = 1*grepl(pattern = "None", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_cigarettes = 1*grepl(pattern = "Cigarettes", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_alcohol = 1*grepl(pattern = "Alcohol", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_vape_juul_or_ecigarettes = 1*grepl(pattern = "Vape pen, JUUL, or e-cigarettes", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_cigars_cigarillos_or_little_cigars = 1*grepl(pattern = "Cigars, cigarillos, or little cigars", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco = 1*grepl(pattern = "Smokeless tobacco/snus/dissolvable tobacco", x = Q19_response, fixed = TRUE),
           Q19_response_cleaned_marijuana_or_cannabis = 1*grepl(pattern = "Marijuana/Cannabis", x = Q19_response, fixed = TRUE)) %>%
    mutate(Q19_response_cleaned_none = replace(Q19_response_cleaned_none, is.na(Q19_response), NA),
           Q19_response_cleaned_cigarettes = replace(Q19_response_cleaned_cigarettes, is.na(Q19_response), NA),
           Q19_response_cleaned_alcohol = replace(Q19_response_cleaned_alcohol, is.na(Q19_response), NA),
           Q19_response_cleaned_vape_juul_or_ecigarettes = replace(Q19_response_cleaned_vape_juul_or_ecigarettes, is.na(Q19_response), NA),
           Q19_response_cleaned_cigars_cigarillos_or_little_cigars = replace(Q19_response_cleaned_cigars_cigarillos_or_little_cigars, is.na(Q19_response), NA),
           Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco = replace(Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco, is.na(Q19_response), NA),
           Q19_response_cleaned_marijuana_or_cannabis = replace(Q19_response_cleaned_marijuana_or_cannabis, is.na(Q19_response), NA)) %>%
    mutate(Q19_response_cleaned_any_nicotine = if_else(Q19_response_cleaned_cigarettes + Q19_response_cleaned_vape_juul_or_ecigarettes + Q19_response_cleaned_cigars_cigarillos_or_little_cigars + Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco >= 1, 1, 0)) %>%
    mutate(Q19_response_cleaned_any_tobacco = if_else(Q19_response_cleaned_cigarettes + Q19_response_cleaned_cigars_cigarillos_or_little_cigars + Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco >= 1, 1, 0)) %>%
    mutate(Q19_response_cleaned_any = abs(Q19_response_cleaned_none - 1)) 
  
  return(cleaned_data_frame)
}

clean_response_cigarette_counts <- function(cleaned_data_frame){
  
  cleaned_data_frame <- clean_response_substance_use_multiple_select(cleaned_data_frame)
  
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(Q20_response_cleaned = case_when(
      Q20_response == "0 (I did not smoke)" ~ 0,
      Q20_response == "Less than 1" ~ 0.5,
      Q20_response == "1" ~ 1,
      Q20_response == "2" ~ 2,
      Q20_response == "3" ~ 3,
      Q20_response == "4" ~ 4,
      Q20_response == "5" ~ 5,
      Q20_response == "6" ~ 6,
      Q20_response == "7" ~ 7,
      Q20_response == "8" ~ 8,
      Q20_response == "9" ~ 9,
      Q20_response == "10" ~ 10,
      Q20_response == "More than 10" ~ 11,
      .default = NULL))
  
  # In this code, set the value of Q20_response_cleaned to zero 
  # if the value of Q19_response_cleaned_cigarettes is zero.
  #
  # This step covers two cases below:
  #   * when the participant responded to Q19 but reported to not use any substances
  #   * when the participant responded to Q19 but reported to use some substance other than cigarettes
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(Q20_response_cleaned = replace(Q20_response_cleaned, Q19_response_cleaned_cigarettes == 0, 0))
  
  return(cleaned_data_frame[["Q20_response_cleaned"]])
}

identify_item_version_cigarette_when_smoked <- function(cleaned_data_frame){
  
  cleaned_data_frame[["Q20_response_cleaned"]] <- clean_response_cigarette_counts(cleaned_data_frame)
  cleaned_data_frame[["status_survey_ema_collapsed"]] <- collapse_survey_ema_status(cleaned_data_frame)
  
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(item_version = case_when(
      (!is.na(Q20_response_cleaned)) & ((Q20_response_cleaned > 0) & (Q20_response_cleaned <= 1)) & (!is.na(Q21_response)) ~ "early_version",
      (!is.na(Q20_response_cleaned)) & ((Q20_response_cleaned > 0) & (Q20_response_cleaned <= 1)) & (!is.na(Q48_response)) ~ "late_version",
      (!is.na(Q20_response_cleaned)) & (Q20_response_cleaned >= 2) & (!is.na(Q23_response)) ~ "early_version",
      (!is.na(Q20_response_cleaned)) & (Q20_response_cleaned >= 2) & (!is.na(Q22_response)) ~ "early_version",
      (!is.na(Q20_response_cleaned)) & (Q20_response_cleaned >= 2) & (!is.na(Q49_response)) ~ "late_version",
      (!is.na(Q20_response_cleaned)) & (Q20_response_cleaned >= 2) & (!is.na(Q50_response)) ~ "late_version",
      (!is.na(Q20_response_cleaned)) & (Q20_response_cleaned == 0) ~ "prior_item_has_response_and_skipped",
      (is.na(Q20_response_cleaned)) & (!is.na(ts_coinflip_mountain)) ~ "prior_item_has_no_response",
      .default = NULL
    ))
  
  return(cleaned_data_frame[["item_version"]])
}

clean_response_vape_puffs <- function(cleaned_data_frame){
  
  cleaned_data_frame <- clean_response_substance_use_multiple_select(cleaned_data_frame)
  
  # Calling suppressWarnings(as.numeric(.)) will let the warning below not
  # be printed in the terminal
  #
  # Warning message:
  #   There was 1 warning in `mutate()`.
  # ℹ In argument: `Q24_response_cleaned = case_when(...)`.
  # Caused by warning in `vec_case_when()`:
  #   ! NAs introduced by coercion 
  
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(Q24_response_cleaned = case_when(
      Q24_response == "0 (I did not vape)" ~ 0,
      Q24_response == "1" ~ 1,
      Q24_response == "2" ~ 2,
      Q24_response == "3" ~ 3,
      Q24_response == "4" ~ 4,
      Q24_response == "5" ~ 5,
      Q24_response == "6" ~ 6,
      Q24_response == "7" ~ 7,
      Q24_response == "8" ~ 8,
      Q24_response == "9" ~ 9,
      Q24_response == "10" ~ 10,
      Q24_response == "11" ~ 11,
      Q24_response == "More than 100" ~ 101,
      .default = suppressWarnings(as.numeric(Q24_response))
      ))
  
  # In this code, set the value of Q24_response_cleaned to zero 
  # if the value of Q19_response_cleaned_vape_juul_or_ecigarettes is zero.
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(Q24_response_cleaned = replace(Q24_response_cleaned, Q19_response_cleaned_vape_juul_or_ecigarettes == 0, 0))
  
  return(cleaned_data_frame[["Q24_response_cleaned"]])
}

identify_item_version_vape_when_puffed <- function(cleaned_data_frame){
  
  cleaned_data_frame[["Q24_response_cleaned"]] <- clean_response_vape_puffs(cleaned_data_frame)
  cleaned_data_frame[["status_survey_ema_collapsed"]] <- collapse_survey_ema_status(cleaned_data_frame)
  
  cleaned_data_frame <- cleaned_data_frame %>% 
    mutate(item_version = case_when(
      (!is.na(Q24_response_cleaned)) & ((Q24_response_cleaned > 0) & (Q24_response_cleaned <= 1)) & (!is.na(Q25_response)) ~ "no_version_change",
      
      (!is.na(Q24_response_cleaned)) & (Q24_response_cleaned >= 2) & (!is.na(Q27_response)) ~ "early_version",
      (!is.na(Q24_response_cleaned)) & (Q24_response_cleaned >= 2) & (!is.na(Q26_response)) ~ "early_version",
      
      (!is.na(Q24_response_cleaned)) & (Q24_response_cleaned >= 2) & (!is.na(Q51_response)) ~ "late_version",
      (!is.na(Q24_response_cleaned)) & (Q24_response_cleaned >= 2) & (!is.na(Q52_response)) ~ "late_version",
      
      (!is.na(Q24_response_cleaned)) & (Q24_response_cleaned == 0) ~ "prior_item_has_response_and_skipped",
      (is.na(Q24_response_cleaned)) & (!is.na(ts_coinflip_mountain)) ~ "prior_item_has_no_response",
      .default = NULL
    ))
  
  return(cleaned_data_frame[["item_version"]])
}


