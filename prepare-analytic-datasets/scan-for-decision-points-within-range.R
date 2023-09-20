source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period.
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

################################################################################
# * Count the number of micro-randomizations in the past 24 hours from
#   current decision point
# * Collect all the decision points that fall within the range of interest
################################################################################
dat_analysis[["ts_coinflip_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_coinflip_mountain")
dat_analysis[["ts_coinflip_past24hrs_local"]] <- dat_analysis[["ts_coinflip_local"]] - hours(24)

dat_analysis[["counts_rand_past24hrs"]] <- NA
dat_analysis[["decision_points_past24hrs"]] <- rep(list(decision_points_within_range = NULL), nrow(dat_analysis))
dat_analysis[["datetimes_coinflip_past24hrs_local"]] <- rep(list(datetimes_within_range = ymd_hms("", tz = "UTC")), nrow(dat_analysis))

list_dat_all_participants <- list()

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  for(j in 1:n_blocks){
    if(!is.na(dat_current_participant[j,"ts_coinflip_local"])){
      UB <- dat_current_participant[j,"ts_coinflip_local"]
      LB <- dat_current_participant[j,"ts_coinflip_past24hrs_local"]
      all_rand_datetimes <- dat_current_participant[["ts_coinflip_local"]]
      is_rand_within_past24hrs <- (all_rand_datetimes >= LB) & (all_rand_datetimes < UB)
      is_rand_within_past24hrs <- replace(is_rand_within_past24hrs, is.na(is_rand_within_past24hrs), FALSE)
      # Count number of micro-randomizations in the past 24 hours
      n_rand_within_past24hrs <- sum(is_rand_within_past24hrs)
      dat_current_participant[j,"counts_rand_past24hrs"] <- n_rand_within_past24hrs
      # Save decision point numbers corresponding to micro-randomizations within the past 24 hours
      if(n_rand_within_past24hrs > 0){
        # Decision point numbers corresponding to micro-randomizations that fall within the time range of interest
        decision_points_within_range <- dat_current_participant[["decision_point"]][is_rand_within_past24hrs]
        dat_current_participant[["decision_points_past24hrs"]][j][["decision_points_within_range"]] <- decision_points_within_range
        # Datetimes corresponding to micro-randomizations that fall within the time range of interest
        datetimes_within_range <- dat_current_participant[["ts_coinflip_local"]][is_rand_within_past24hrs]
        dat_current_participant[["datetimes_coinflip_past24hrs_local"]][j][["datetimes_within_range"]] <- datetimes_within_range
      }
    }
  }
  list_dat_all_participants <- append(list_dat_all_participants, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_dat_all_participants)

################################################################################
# Count the number of triggered EMAs and number of fully completed EMAs 
# in the past 24 hours from current decision point
################################################################################
dat_analysis[["counts_ema_triggered_past24hrs"]] <- NA
dat_analysis[["counts_ema_completed_past24hrs"]] <- NA  # Note that this variable does not count those EMAs which were PARTIALLY completed
list_all_dat <- list()

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
        
        is_ema_triggered <- if_else(!is.na(dat_within_range[["ts_ema_triggered_mountain"]]), TRUE, FALSE)
        dat_current_participant[j,"counts_ema_triggered_past24hrs"] <- sum(is_ema_triggered)
        
        is_ema_completed <- dat_within_range[["status_survey_ema"]] == "completed"
        is_ema_completed <- replace(is_ema_completed, is.na(is_ema_completed), FALSE)
        dat_current_participant[j,"counts_ema_completed_past24hrs"] <- sum(is_ema_completed)
      }else{
        dat_current_participant[j,"counts_ema_triggered_past24hrs"] <- 0
        dat_current_participant[j,"counts_ema_completed_past24hrs"] <- 0
      }
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

################################################################################
# In this for loop, we identify the most recent eligible decision point BEFORE
# the current decision point
################################################################################
dat_analysis[["eligibility"]] <- NA
dat_analysis[["decision_points_most_recent_eligible"]] <- NA
list_all_dat <- list()

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant) %>% mutate(eligibility = if_else(!is.na(ts_coinflip_mountain), 1, 0))
  n_blocks <- nrow(dat_current_participant)
  arr_all_elig_indices <- which(dat_current_participant[["eligibility"]] == 1)
  
  for(current_row_number in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[current_row_number,"ts_coinflip_mountain"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      arr_found <- arr_all_elig_indices < current_row_number
      any_found <- if_else(sum(arr_found) == 0, 0, 1)
      if(any_found == 1){
        these_indices_found <- arr_all_elig_indices[arr_found]
        the_matched_index <- max(these_indices_found)
      }else{
        the_matched_index <- NA_real_
      }
      dat_current_participant[["decision_points_most_recent_eligible"]][current_row_number] <- the_matched_index
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

################################################################################
# Select the columns you need
################################################################################
dat_analysis <- dat_analysis %>% 
  rename(LB_past24hrs_local = ts_coinflip_past24hrs_local, UB_past24hrs_local = ts_coinflip_local)

dat_analysis_with_metadata <- dat_analysis %>% 
  select(mars_id, decision_point, 
         eligibility, decision_points_most_recent_eligible,
         LB_past24hrs_local, UB_past24hrs_local,
         counts_rand_past24hrs, 
         decision_points_past24hrs, datetimes_coinflip_past24hrs_local,
         counts_ema_triggered_past24hrs, counts_ema_completed_past24hrs)

dat_analysis_with_no_metadata <- dat_analysis %>% 
  select(mars_id, decision_point, 
         eligibility, decision_points_most_recent_eligible,
         decision_points_past24hrs, 
         counts_rand_past24hrs)

################################################################################
# Save output
################################################################################
saveRDS(dat_analysis_with_no_metadata, file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))
saveRDS(dat_analysis_with_metadata, file = file.path(path_manipulated_data, "scanned_decision_points_within_range_with_metadata.rds"))

