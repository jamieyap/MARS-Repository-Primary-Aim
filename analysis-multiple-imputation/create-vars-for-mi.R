rm(list = ls())

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
# Keep track of column names you will want to keep
################################################################################
keep_these_columns_for_analysis <- list()

################################################################################
# Read in dataset which already identifies micro-randomizations in the past
# 24 hours
################################################################################
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))
new_cols <- colnames(scanned_decision_points_within_range)
dat_analysis <- left_join(x = dat_analysis, 
                          y = scanned_decision_points_within_range, 
                          by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- dat_analysis %>% select(all_of(new_cols), everything())

################################################################################
# Keep track of column names you will want to keep
################################################################################
keep_these_columns_for_analysis <- list()

################################################################################
# Re-calculate the primary proximal outcome in preparation for steps below
# Re-calculate treatment indicator variables in preparation for the steps below
################################################################################
dat_analysis[["Y"]] <- construct_primary_proximal_outcome(cleaned_data_frame = dat_analysis, q1_var_name = "Q1_response", q2_var_name = "Q2_response", q3_var_name = "Q3_response")
dat_analysis[["coinflip"]] <- if_else((dat_analysis[["A"]] == "mars") | (dat_analysis[["A"]] == "low_effort"), 1, 0)
dat_analysis[["is_high_effort"]] <- if_else(dat_analysis[["A"]] == "mars", 1, 0)
dat_analysis[["is_low_effort"]] <- if_else(dat_analysis[["A"]] == "low_effort", 1, 0)

################################################################################
# Create time-varying moderators: aggregate measure over the past 24 hours
################################################################################
lookup <- c(Y_sum_past24hrs = "Y_sum_past24hrs",
            Y_nreported_past24hrs = "Y_nreported_past24hrs",
            is_high_effort_sum_past24hrs = "is_high_effort_sum_past24hrs",
            is_low_effort_sum_past24hrs = "is_low_effort_sum_past24hrs")

list_all_dat <- list()
dat_analysis[["Y_sum_past24hrs"]] <- NA
dat_analysis[["is_high_effort_sum_past24hrs"]] <- NA
dat_analysis[["is_low_effort_sum_past24hrs"]] <- NA

dat_analysis[["Y_nreported_past24hrs"]] <- NA

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
        dat_current_participant[j,"is_high_effort_sum_past24hrs"] <- sum(!is.na(dat_within_range[["is_high_effort"]]))
        dat_current_participant[j,"is_low_effort_sum_past24hrs"] <- sum(!is.na(dat_within_range[["is_low_effort"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"Y_nreported_past24hrs"]){
          dat_current_participant[j,"Y_sum_past24hrs"] <- sum(dat_within_range[["Y"]], na.rm = TRUE)
        }
        
      }else{
        dat_current_participant[j,"Y_nreported_past24hrs"] <- 0
      }
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Select only the columns you will need
################################################################################
keep_these_columns_for_analysis <- unlist(keep_these_columns_for_analysis)
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(keep_these_columns_for_analysis))
saveRDS(dat_analysis, file = file.path(path_multiple_imputation_data, "dat_mars_mi_time_varying_covariates.rds"))

