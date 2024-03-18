rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_mars_basic <- readRDS(file = file.path(path_manipulated_data, "dat_mars_basic.rds"))
dat_mars_derived_time_vars <- readRDS(file = file.path(path_manipulated_data, "dat_mars_derived_time_vars.rds"))
dat_mars_time_varying_noise_reduction_vars <- readRDS(file = file.path(path_manipulated_data, "dat_mars_time_varying_noise_reduction_vars.rds"))
dat_mars_time_varying_moderators <- readRDS(file = file.path(path_manipulated_data, "dat_mars_time_varying_moderators.rds"))

dat_analysis <- left_join(x = dat_mars_basic, y = dat_mars_derived_time_vars, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- left_join(x = dat_analysis, y = dat_mars_time_varying_noise_reduction_vars, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- left_join(x = dat_analysis, y = dat_mars_time_varying_moderators, by = join_by(mars_id == mars_id, decision_point == decision_point))

all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

################################################################################
# Keep track of column names you will want to keep
################################################################################
keep_these_columns_for_analysis <- list() 

################################################################################
# Read in dataset which already identifies micro-randomizations in the past
# 24 hours.
################################################################################
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))

################################################################################
# Save the earliest decision point within the past 24 hours and most recent
# eligible decision point. Here, NULL or NA values represent the fact that
# the value is undefined, rather than missing. We code NULL or NA values here
# as -1.
################################################################################
scanned_decision_points_within_range[["matched_24hrs"]] <- lapply(scanned_decision_points_within_range[["decision_points_past24hrs"]],  
                                                                  function(x){
                                                                    cond <- (!is.null(x[[1]]))
                                                                    if(cond){
                                                                      out <- min(x[[1]])
                                                                    }else{
                                                                      out <- -1
                                                                    }
                                                                  })

scanned_decision_points_within_range[["matched_24hrs"]] <- unlist(scanned_decision_points_within_range[["matched_24hrs"]])
scanned_decision_points_within_range[["matched_recent"]] <- scanned_decision_points_within_range[["decision_points_most_recent_eligible"]]
scanned_decision_points_within_range[["matched_recent"]] <- replace(scanned_decision_points_within_range[["matched_recent"]], is.na(scanned_decision_points_within_range[["matched_recent"]]), -1)

lookup <- c(matched_24hrs = "matched_24hrs", matched_recent = "matched_recent")
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Total number of micro-randomizations in the past 24 hours prior to the 
# current micro-randomization (not including the current micro-randomization)
################################################################################

# Note that counts_rand_past24hrs is only calculated when eligibility == 1. 
# Since counts_rand_past24hrs takes on a value of NA when eligibility == 0, 
# elig24hrs will also take on a value of NA when eligibility == 0.
scanned_decision_points_within_range <- scanned_decision_points_within_range %>%
  mutate(elig24hrs = if_else(counts_rand_past24hrs > 0, 1, 0)) %>%
  mutate(elig24hrs = if_else(eligibility == 0, -1, elig24hrs))

################################################################################
# Bring in variables from scanned_decision_points_within_range into dat_analysis
# as a preparatory step for creating variables that are operationalized as an
# aggregate of data in the past 24 hours
################################################################################

# Remove columns that are already in dat_analysis prior to merging
scanned_decision_points_within_range <- scanned_decision_points_within_range %>%
  select(-counts_rand_past24hrs, -eligibility)

dat_analysis <- left_join(x = dat_analysis, 
                          y = scanned_decision_points_within_range, 
                          by = join_by(mars_id == mars_id, decision_point == decision_point))

################################################################################
# Create aggregate measure over the past 24 hours
################################################################################
lookup <- c(Y_nreported_past24hrs = "Y_nreported_past24hrs",
            Y_sum_past24hrs = "Y_sum_past24hrs",
            is_high_effort_sum_past24hrs = "is_high_effort_sum_past24hrs",
            is_low_effort_sum_past24hrs = "is_low_effort_sum_past24hrs")

list_all_dat <- list()
dat_analysis[["Y_nreported_past24hrs"]] <- 0
dat_analysis[["Y_sum_past24hrs"]] <- NA
dat_analysis[["coinflip_sum_past24hrs"]] <- NA
dat_analysis[["is_high_effort_sum_past24hrs"]] <- NA
dat_analysis[["is_low_effort_sum_past24hrs"]] <- NA

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
        dat_current_participant[j,"Y_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["Y"]]))
        dat_current_participant[j,"coinflip_sum_past24hrs"] <- sum(!is.na(dat_within_range[["coinflip"]]))
        dat_current_participant[j,"is_high_effort_sum_past24hrs"] <- sum(!is.na(dat_within_range[["is_high_effort"]]))
        dat_current_participant[j,"is_low_effort_sum_past24hrs"] <- sum(!is.na(dat_within_range[["is_low_effort"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"Y_nreported_past24hrs"]){
          dat_current_participant[j,"Y_sum_past24hrs"] <- sum(dat_within_range[["Y"]], na.rm = TRUE)
        }
      }
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create aggregate measure over the past 24 hours
# Note that these aggregate measures are only taken over decision points which
# were eligible for micro-randomization
################################################################################
lookup <- c(quick_survey_nreported_past24hrs = "quick_survey_nreported_past24hrs",
            negative_affect_sum_past24hrs = "negative_affect_sum_past24hrs",
            cig_available_sum_past24hrs = "cig_available_sum_past24hrs",
            dichotomized_2qs_response_sum_past24hrs = "dichotomized_2qs_response_sum_past24hrs")

list_all_dat <- list()
dat_analysis[["quick_survey_nreported_past24hrs"]] <- 0
dat_analysis[["negative_affect_sum_past24hrs"]] <- NA
dat_analysis[["cig_available_sum_past24hrs"]] <- NA
dat_analysis[["dichotomized_2qs_response_sum_past24hrs"]] <- NA

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
        tmp <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["status_survey_2qs_collapsed"]]
        dat_current_participant[j,"quick_survey_nreported_past24hrs"] <- sum(tmp == "fully_completed", na.rm = TRUE)
        
        if(n_rand_past24hrs == dat_current_participant[j,"quick_survey_nreported_past24hrs"]){
          dat_current_participant[j,"negative_affect_sum_past24hrs"] <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["negative_affect"]] %>% sum(., na.rm = TRUE)
          dat_current_participant[j,"cig_available_sum_past24hrs"] <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["cig_available"]] %>% sum(., na.rm = TRUE)
          dat_current_participant[j,"dichotomized_2qs_response_sum_past24hrs"] <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["dichotomized_2qs_response"]] %>% sum(., na.rm = TRUE)
        }
      }
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Bring in variables on app usage at current block
################################################################################
dat_app_usage <- readRDS(file = file.path(path_app_usage_data, "app_usage_for_mainpipeline_updated0212.rds")) 

dat_app_usage <- dat_app_usage %>%
  filter(!is.na(A)) %>%
  filter(mars_id %in% all_ids) %>%
  mutate(emi_resp = replace(emi_resp, is.na(emi_resp), "none")) %>%
  mutate(emi_resp_indicator = if_else(((A == "low_effort") | (A == "mars")) & ((read_tips == 1) | (activ_done == 1)), 1, 0)) %>%
  select(mars_id, decision_point, A, emi_resp, emi_resp_indicator) %>%
  select(mars_id, decision_point, emi_resp_indicator)

# Finally, bring in information on app usage
dat_analysis <- left_join(x = dat_analysis, y = dat_app_usage, by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- dat_analysis %>% 
  select(mars_id, participant_id, cluster_id, decision_point, 
         eligibility, coinflip, is_high_effort, is_low_effort, emi_resp_indicator, everything())

lookup <- c(emi_resp_indicator = "emi_resp_indicator")
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

###############################################################################
# App usage in the past 24 hours
###############################################################################
list_all_dat <- list()
dat_analysis[["emi_resp_indicator_sum_past24hrs"]] <- NA
dat_analysis[["is_high_effort_sum_past24hrs"]] <- NA
dat_analysis[["is_low_effort_sum_past24hrs"]] <- NA
dat_analysis[["coinflip_sum_past24hrs"]] <- NA

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
        
        tmp <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["emi_resp_indicator"]]
        dat_current_participant[j,"emi_resp_indicator_sum_past24hrs"] <- sum(tmp)
        
        tmp <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["is_high_effort"]]
        dat_current_participant[j,"is_high_effort_sum_past24hrs"] <- sum(tmp)
        
        tmp <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["is_low_effort"]]
        dat_current_participant[j,"is_low_effort_sum_past24hrs"] <- sum(tmp)
        
        tmp <- dat_within_range %>% filter(!is.na(ts_coinflip_mountain)) %>% .[["coinflip"]]
        dat_current_participant[j,"coinflip_sum_past24hrs"] <- sum(tmp)
      }
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)

lookup <- c(emi_resp_indicator_sum_past24hrs = "emi_resp_indicator_sum_past24hrs",
            is_high_effort_sum_past24hrs = "is_high_effort_sum_past24hrs",
            is_low_effort_sum_past24hrs = "is_low_effort_sum_past24hrs",
            coinflip_sum_past24hrs = "coinflip_sum_past24hrs")
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Select only the columns you will need
################################################################################
keep_these_columns_for_analysis <- unlist(keep_these_columns_for_analysis)
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, eligibility, elig24hrs, all_of(keep_these_columns_for_analysis))

################################################################################
# Save data frame
################################################################################
saveRDS(dat_analysis, file = file.path(path_manipulated_data, "dat_mars_time_varying_covariates.rds"))

