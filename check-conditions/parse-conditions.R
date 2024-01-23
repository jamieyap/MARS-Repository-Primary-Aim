rm(list = ls())

source("paths.R")
library(tidyverse)

list_all_system_log <- readRDS(file = file.path(path_manipulated_data, "list_all_system_log.rds"))

###############################################################################
# Go through each participant's log file and parse strings that provide
# information used to decide whether micro-randomization,
# 2qs administration and EMA administration ought to occur.
###############################################################################
list_parsed_conditions <- list()

for(current_participant in 1:length(list_all_system_log)){
  dat_current_participant <- list_all_system_log[[current_participant]]
  dat_current_participant <- dat_current_participant %>%
    filter(grepl(pattern = "is_privacy_on()==false && is_driving(now()-time_offset(00:05:00)", .data[["V6"]], fixed = TRUE)) 
  
  list_checked_conditions <- strsplit(x = dat_current_participant[["V6"]], split = ";")
  list_privacy <- lapply(X = list_checked_conditions, FUN = function(x){x[1]})
  list_driving <- lapply(X = list_checked_conditions, FUN = function(x){paste(x[2], x[3], sep = " until")})
  dat_current_participant[["extracted_raw_privacy"]] <- unlist(list_privacy)
  dat_current_participant[["extracted_raw_driving"]] <- unlist(list_driving)
  
  dat_current_participant <- dat_current_participant %>%
    mutate(V1 = as.numeric(V1)) %>%
    mutate(ts_condition_check_completed_utc = as_datetime(x = V1/1000, tz = "UTC"))
  
  dat_current_participant[["ts_condition_check_completed_mountain"]] <- with_tz(time = dat_current_participant[["ts_condition_check_completed_utc"]], tzone = "US/Mountain")
  
  dat_current_participant <- dat_current_participant %>%
    mutate(olson = case_when(
      V2/(1000*60*60) == -5 ~ "US/Eastern", 
      V2/(1000*60*60) == -6 ~ "US/Central", 
      V2/(1000*60*60) == -7 ~ "US/Mountain", 
      V2/(1000*60*60) == -8 ~ "US/Pacific",
      .default = NULL
    )) 
  
  dat_current_participant <- dat_current_participant %>%
    mutate(is_2qs = if_else(grepl(pattern = "EMA-TAILOR", .data[["V5"]], fixed = TRUE), 1, 0)) %>%
    mutate(is_rand = if_else(grepl(pattern = "EMI-RANDOM", .data[["V5"]], fixed = TRUE), 1, 0)) %>%
    mutate(is_ema = if_else(grepl(pattern = "EMA-RANDOM", .data[["V5"]], fixed = TRUE), 1, 0)) %>%
    mutate(evaluated_true = grepl(pattern = "Condition=true", .data[["extracted_raw_privacy"]], fixed = TRUE))
  
  list_parsed_conditions <- append(list_parsed_conditions, list(dat_current_participant))
}

dat_parsed_conditions <- bind_rows(list_parsed_conditions)

###############################################################################
# Handle participants having what appears to be a glitch in the recording of
# their timezone (i.e., handling the 'olson' variable)
# Note that this is the same correction made in clean-up-atypical-sequences.R
###############################################################################
dat_parsed_conditions <- dat_parsed_conditions %>%
  mutate(olson = replace(olson, mars_id == "mars_147", "US/Pacific")) %>%
  mutate(olson = replace(olson, mars_id == "mars_152", "US/Pacific"))

###############################################################################
# Remove exact duplicates
###############################################################################
dat_parsed_conditions <- dat_parsed_conditions %>% 
  mutate(is_duplicate = duplicated(.)) %>%
  filter(is_duplicate == FALSE) %>%
  select(-is_duplicate)

###############################################################################
# Create binary indicators using parsed information on the context
# surrounding the decision on whether micro-randomization,
# 2qs administration and EMA administration ought to occur.
###############################################################################
dat_parsed_conditions <- dat_parsed_conditions %>%
  mutate(privacy_data_not_found = (grepl(pattern = "[data not found]", .data[["extracted_raw_privacy"]], fixed = TRUE) | grepl(pattern = "[datasource not found]", .data[["extracted_raw_privacy"]], fixed = TRUE))) %>%
  mutate(privacy_activated = grepl(pattern = "privacy enabled", .data[["extracted_raw_privacy"]], fixed = TRUE)) %>%
  mutate(privacy_not_activated = (grepl(pattern = "[privacy status=false]", .data[["extracted_raw_privacy"]], fixed = TRUE) | grepl(pattern = "[privacy is not enabled]", .data[["extracted_raw_privacy"]], fixed = TRUE) | grepl(pattern = "[privacy_time < current_time]", .data[["extracted_raw_privacy"]], fixed = TRUE)))

dat_parsed_conditions <- dat_parsed_conditions %>%
  mutate(driving_data_not_found = (grepl(pattern = "[data not found]", .data[["extracted_raw_driving"]], fixed = TRUE) | grepl(pattern = "[datasource not found]", .data[["extracted_raw_driving"]], fixed = TRUE))) %>%
  mutate(driving_detected = grepl(pattern = "true", .data[["extracted_raw_driving"]], fixed = TRUE)) %>%
  mutate(driving_not_detected = grepl(pattern = "false", .data[["extracted_raw_driving"]], fixed = TRUE))

###############################################################################
# One row was dropped upon running the next line
# In this row, V6 does not contain complete info 
# (e.g., whether privacy was activated and 
# whether driving was detected was not recorded in this row)
###############################################################################
dat_parsed_conditions <- dat_parsed_conditions %>% filter(!((is_2qs == 0) & (is_rand == 0) & (is_ema == 0)))

###############################################################################
# Grab selected columns
###############################################################################
dat_parsed_conditions <- dat_parsed_conditions %>%
  mutate(condition_id = 1:nrow(.)) %>%
  select(mars_id, olson, ts_condition_check_completed_mountain,
         is_2qs, is_rand, is_ema,
         condition_id, evaluated_true,
         privacy_data_not_found, privacy_activated, privacy_not_activated,
         driving_data_not_found, driving_detected, driving_not_detected,
         V6, extracted_raw_driving, extracted_raw_privacy) %>%
  rename(condition_check_raw = V6)

###############################################################################
# Save output
###############################################################################
saveRDS(dat_parsed_conditions, file = file.path(path_manipulated_data, "dat_parsed_conditions.rds"))

