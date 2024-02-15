################################################################################
# In this script, we use parsed data as a starting point for implementing a 
# decision rule for creating a binary proximal indicator of cigarette smoking 
# within ~1 hour of micro-randomization.
################################################################################

rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################

parsed_cigarette_smoking_times <- readRDS(file = file.path(path_proximal_smoking_pipeline_data, "parsed_cigarette_smoking_times.rds"))
dat_analysis <- parsed_cigarette_smoking_times

################################################################################
# Decision rule for determining date-time to associate with response
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(ts_ema_local = case_when(
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!is.na(Q21_ts_finish_time_local)) ~ Q21_ts_finish_time_local,  # EMA item used by one batch of participants to report when they smoked "that" cigarette 
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!is.na(Q48_ts_finish_time_local)) ~ Q48_ts_finish_time_local,  # EMA item used by another batch of participants to report when they smoked "that" cigarette
    (cigarette_counts >= 2) & (!is.na(Q22_ts_finish_time_local)) ~ Q22_ts_finish_time_local,  # EMA item used by one batch of participants to report when the moked the "most recent" cigarette
    (cigarette_counts >= 2) & (!is.na(Q50_ts_finish_time_local)) ~ Q50_ts_finish_time_local,  # EMA item used by another batch of participants to report when the moked the "most recent" cigarette
    .default = NULL
  ))

################################################################################
# Employ decision rule which uses information from reported time when
# cigarettes smoked
################################################################################

dat_analysis <- dat_analysis %>%
  mutate(substance_is_cigarettes_new = case_when(
    cigarette_counts == 0 ~ 0,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (cigs_one_datetime_local > ts_coinflip_local) & (cigs_one_datetime_local <= ts_ema_local) ~ 1,
    (cigarette_counts > 0) & (cigarette_counts <= 1) & (!((cigs_one_datetime_local > ts_coinflip_local) & (cigs_one_datetime_local <= ts_ema_local))) ~ 0,
    (cigarette_counts >= 2) & (((cigs_many_first_datetime_local > ts_coinflip_local) & (cigs_many_first_datetime_local <= ts_ema_local)) | ((cigs_many_last_datetime_local > ts_coinflip_local) & (cigs_many_last_datetime_local <= ts_ema_local))) ~ 1,
    (cigarette_counts >= 2) & ((!((cigs_many_first_datetime_local > ts_coinflip_local) & (cigs_many_first_datetime_local <= ts_ema_local))) & (!((cigs_many_last_datetime_local > ts_coinflip_local) & (cigs_many_last_datetime_local <= ts_ema_local)))) ~ 0,
    .default = NA
  ))

################################################################################
# Read in dataset which already identifies micro-randomizations in the past
# 24 hours
################################################################################
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))

dat_analysis <- left_join(x = dat_analysis, 
                          y = scanned_decision_points_within_range, 
                          by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Calculate aggregate measure in terms of past 24 hours
################################################################################
list_all_dat <- list()
dat_analysis[["substance_is_cigarettes_new_sum_past24hrs"]] <- NA
dat_analysis[["substance_is_cigarettes_new_nreported_past24hrs"]] <- NA

all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[j,"ts_coinflip_mountain"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      n_rand_past24hrs <- dat_current_participant[j,"counts_rand_past24hrs"]
      
      if(n_rand_past24hrs > 0){
        dp_within_range <- dat_current_participant[j,"decision_points_past24hrs"][[1]]
        dat_within_range <- dat_current_participant %>% filter(decision_point %in% dp_within_range)
        
        dat_current_participant[j,"substance_is_cigarettes_new_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["substance_is_cigarettes_new"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"substance_is_cigarettes_new_nreported_past24hrs"]){
          dat_current_participant[j,"substance_is_cigarettes_new_sum_past24hrs"] <- sum(dat_within_range[["substance_is_cigarettes_new"]], na.rm = TRUE)
        }
        
      }  # This if-then statement only executes if a block had any micro-randomization in the past 24 hours PRIOR TO the current micro-randomization
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

################################################################################
# Save data frame
################################################################################
dat_analysis_with_metadata <- dat_analysis
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, substance_is_cigarettes_new, substance_is_cigarettes_new_sum_past24hrs)

saveRDS(dat_analysis_with_metadata, file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_cigarette_smoking_with_metadata.rds"))
saveRDS(dat_analysis, file = file.path(path_proximal_smoking_pipeline_data, "dat_mars_proximal_cigarette_smoking.rds"))

