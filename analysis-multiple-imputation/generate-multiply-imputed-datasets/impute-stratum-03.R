rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Imputation number
###############################################################################
mi_dataset_num <- .__current_idx  # Change the right hand side of this line if not running within a loop
use_maxit_value <- .__par_maxit_value
st_num <- 3
which_penalty <- "BIC"  # Can be set to either "AIC" or "BIC"

current_dp_value <- .__current_dp  # Change the right hand side of this line if not running within a loop
suffix <- paste("_dp" ,  current_dp_value, sep = "")
suffix_lag1 <- paste("_dp" ,  current_dp_value - 1, sep = "")

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(pROC)
library(MASS)
library(tidyverse)
library(mice)

# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

# A function for throwing an error
check_throw_error <- function(x) {
  stopifnot(x == TRUE)
}

###############################################################################
# Set up dataset in preparation for imputation at the current decision point
###############################################################################

# Read in completed dataset from previous time-point
dat_imputed_stratum_01 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum1.rds"))
dat_imputed_stratum_02 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum2.rds"))
dat_imputed_stratum_01_current_dp <- dat_imputed_stratum_01 %>% filter(decision_point == current_dp_value)
dat_imputed_stratum_02_current_dp <- dat_imputed_stratum_02 %>% filter(decision_point == current_dp_value)
n_participants_meet_sparse_restrictions_stratum_01_current_dp <- nrow(dat_imputed_stratum_01_current_dp)
n_participants_meet_sparse_restrictions_stratum_02_current_dp <- nrow(dat_imputed_stratum_02_current_dp)

# Read in time-varying variables and select the columns pertaining to the current decision point
if(current_dp_value == 2){
  # Grab completed data from baseline
  dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
  
  # Grab completed data from first decision point
  dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
  dat_wide_completed_dp1 <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with("_dp1"))
  
  lookup_table <- dat_imputed_stratum_01 %>% 
    filter(decision_point == 1) %>% 
    select(replicate_id, participant_id, Y, cigarette_counts, src_scored, cigarette_availability)
  
  dat_wide_completed_dp1 <- left_join(x = dat_wide_completed_dp1, y = lookup_table, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
  
  dat_wide_completed_dp1 <- dat_wide_completed_dp1 %>% 
    mutate(Y_dp1 = Y, cigarette_counts_dp1 = cigarette_counts, src_scored_dp1 = src_scored, cigarette_availability_dp1 = cigarette_availability) %>% 
    select(-Y, -cigarette_counts, -src_scored, -cigarette_availability)
  
  # Merge completed data from baseline and the first decision point
  dat_wide_from_prior_step <- left_join(x = dat_wide_completed_baseline, y = dat_wide_completed_dp1, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
}else{
  # Grab completed data from first decision point
  # No need to read in dat_wide_completed_baseline since this will already have the imputed baseline variables
  dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num,  paste("dat_wide_completed_dp", current_dp_value - 1, ".rds", sep = "")))
}

dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with(suffix))
dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

###############################################################################
# Step 0. Update completed dataset -- Y at the prior decision point
###############################################################################
dat_wide[[paste("Y_lag1", suffix, sep = "")]] <- dat_wide[[paste("Y", suffix_lag1, sep = "")]]
dat_wide[[paste("Y_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("Y_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- number of cigarettes smoked
# reported at the prior decision point
###############################################################################
dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]] <- dat_wide[[paste("cigarette_counts", suffix_lag1, sep = "")]]
dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("cigarette_counts_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- score of self-regulatory capacity
# at the prior decision point
###############################################################################
dat_wide[[paste("src_scored_lag1", suffix, sep = "")]] <- dat_wide[[paste("src_scored", suffix_lag1, sep = "")]]
dat_wide[[paste("src_scored_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("src_scored_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- cigarette availability
# reported at the prior decision point
###############################################################################
dat_wide[[paste("cigarette_availability_lag1", suffix, sep = "")]] <- dat_wide[[paste("cigarette_availability", suffix_lag1, sep = "")]]
dat_wide[[paste("cigarette_availability_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("cigarette_availability_lag1", suffix, sep = "")]])

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_wide[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)
minimum_replicate_id <- min(all_replicate_ids)

###############################################################################
# Step 0. Update completed dataset
###############################################################################
if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, paste("any_recent_eligible_dp", suffix, sep = "")] == 1){
        matched_dp <- dat_current[this_participant, paste("matched_recent", suffix, sep = "")]
        matched_value <- dat_current[this_participant, paste("Y_dp", matched_dp, sep = "")]
        dat_current[this_participant, paste("engagement_most_recent_eligible", suffix, sep = "")] <- matched_value
      }
    }
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, paste("any_recent_eligible_dp", suffix, sep = "")] == 1){
      matched_dp <- dat_wide[this_participant, paste("matched_recent", suffix, sep = "")]
      matched_value <- dat_wide[this_participant, paste("Y_dp", matched_dp, sep = "")]
      matched_value <- as.numeric(matched_value) - 1
      dat_wide[this_participant, paste("engagement_most_recent_eligible", suffix, sep = "")] <- matched_value
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- sum of Y in past 24 hours
###############################################################################
this_variable <- "Y"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    # Recall that the original variable is a factor with two levels: 0 and 1
    # Thus, when applying as.numeric() the resulting variable takes on values 1 and 2
    # So, we subtract by 1 so that the resulting variable takes on values of 0 and 1
    dat_wide[[variable_name_transformed]] <- as.numeric(dat_wide[[variable_name_original]]) - 1
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value
      }
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- sum of cigarette counts in past 24 hours
###############################################################################
this_variable <- "cigarette_counts"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_sum_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value
      }
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- mean of src_scored
# in past 24 hours
###############################################################################
this_variable <- "src_scored"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value
      }
    }
  }
}

###############################################################################
# Step 0. Update completed dataset -- mean of cigarette availability
# in past 24 hours
###############################################################################
this_variable <- "cigarette_availability"
this_indicator <- "eligibility"

if(maximum_replicate_id > 0){
  list_dat_all <- list()
  for(idx in minimum_replicate_id:maximum_replicate_id){
    dat_current <- dat_wide %>% filter(replicate_id == idx)
    
    # This loop checks all decision points prior to the current decision point
    for(k in 1:(current_dp_value - 1)){
      variable_name_original <- paste(this_variable, "_dp", k, sep = "")
      variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
      
      dat_current[[variable_name_transformed]] <- dat_current[[variable_name_original]]
      
      # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
      dat_current[[variable_name_transformed]] <- replace(dat_current[[variable_name_transformed]], dat_current[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
    }
    
    variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
    variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
    variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
    variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
    
    dat_current[[variable_name_past24hrs]] <- -1
    
    for(this_participant in 1:nrow(dat_current)){
      if(dat_current[this_participant, variable_name_indicator_now] == 1){
        if(dat_current[this_participant, variable_name_indicator_past24hrs] == 1){
          # What are all the eligible decision points in the past 24 hours prior to the current decision point
          matched_dp <- dat_current[this_participant, variable_name_matched_decision_point]
          all_indices_past24hrs <- matched_dp:(current_dp_value-1)
          all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
          
          # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
          all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
          
          # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
          all_summands <- dat_current[this_participant, all_names_history_eligibility] * dat_current[this_participant, all_names_history_this_variable]
          resulting_value <- sum(all_summands)
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value
        }
      }
    }
    
    list_dat_all <- append(list_dat_all, list(dat_current))
  }
  dat_wide <- bind_rows(list_dat_all)
}else{
  # This loop checks all decision points prior to the current decision point
  for(k in 1:(current_dp_value - 1)){
    variable_name_original <- paste(this_variable, "_dp", k, sep = "")
    variable_name_transformed <- paste(this_variable, "_dp", k, "_numeric_version", sep = "")
    
    dat_wide[[variable_name_transformed]] <- dat_wide[[variable_name_original]]
    
    # If not eligible, replace NA by -1 to facilitate ease of computation in the step below.
    dat_wide[[variable_name_transformed]] <- replace(dat_wide[[variable_name_transformed]], dat_wide[[paste("eligibility_dp", k, sep = "")]] == 0, -1)
  }
  
  variable_name_past24hrs <- paste(this_variable, "_mean_past24hrs", suffix, sep = "")
  variable_name_indicator_now <- paste(this_indicator, suffix, sep = "")
  variable_name_indicator_past24hrs <- paste("elig24hrs", suffix, sep = "")
  variable_name_matched_decision_point <- paste("matched_24hrs", suffix, sep = "")
  
  dat_wide[[variable_name_past24hrs]] <- -1
  
  for(this_participant in 1:nrow(dat_wide)){
    if(dat_wide[this_participant, variable_name_indicator_now] == 1){
      if(dat_wide[this_participant, variable_name_indicator_past24hrs] == 1){
        # What are all the eligible decision points in the past 24 hours prior to the current decision point
        matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
        all_indices_past24hrs <- matched_dp:(current_dp_value-1)
        all_names_history_eligibility <- paste(this_indicator, "_dp", all_indices_past24hrs, sep = "")
        
        # What are all the values of this_variable at all the eligible decision points in the past 24 hours prior to the current decision point
        all_names_history_this_variable <- paste(this_variable, "_dp", all_indices_past24hrs, "_numeric_version", sep = "")
        
        # Take the sum of the value of this_variable among all eligible decision points in the past 24 hours prior to the current decision point
        all_summands <- dat_wide[this_participant, all_names_history_eligibility] * dat_wide[this_participant, all_names_history_this_variable]
        resulting_value <- sum(all_summands)
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value
      }
    }
  }
}

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#                                                                             #
###############################################################################
my_list <- list("cigarette_availability" = NULL,
                "src_scored" = NULL,
                "cigarette_counts" = NULL,
                "Y" = NULL)

this_outcome <- "cigarette_availability"
my_list[[this_outcome]] <- c("age", "is_male", 
                             "income_val", "nd_mean", "food_security_mean",
                             "has_partner", "sni_count", "sni_active", "sni_people",
                             paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""), 
                             paste(this_outcome, "_mean_past24hrs", suffix, sep = ""))

this_outcome <- "src_scored"
my_list[[this_outcome]] <- c("srq_mean", "se_social", "se_habit", "se_negaff",
                             "has_partner", "sni_count", "sni_active", "sni_people",
                             paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""), 
                             paste(this_outcome, "_mean_past24hrs", suffix, sep = ""),
                             paste("Y", "_lag1", suffix, sep = ""), 
                             paste("Y", "_sum_past24hrs", suffix, sep = ""),
                             paste(c("cigarette_availability"), suffix, sep = ""))

this_outcome <- "cigarette_counts"
my_list[[this_outcome]] <- c("age", "is_male", "income_val",
                             "baseline_tobacco_history", "Nicotine_dep",
                             paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""), 
                             paste(this_outcome, "_sum_past24hrs", suffix, sep = ""),
                             paste("Y", "_lag1", suffix, sep = ""), 
                             paste("Y", "_sum_past24hrs", suffix, sep = ""),
                             paste(c("cigarette_availability", "src_scored"), suffix, sep = ""))

this_outcome <- "Y"
my_list[[this_outcome]] <- c("age", "is_male", "income_val",
                             paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                             paste(this_outcome, "_lag1", suffix, sep = ""), 
                             paste(this_outcome, "_sum_past24hrs", suffix, sep = ""),
                             paste(c("any_response_2qs", "any_app_usage_preblock", "total_app_usage_time_spent_preblock"), suffix, sep = ""),
                             paste(c("cigarette_availability", "src_scored", "cigarette_counts"), suffix, sep = ""))

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#    (This is a "smaller" set of possible predictors we will consider         #
#     in case fitting a model with a "larger" set of possible predictors      #
#     above does not converge)                                                #
#                                                                             #
###############################################################################
my_list2 <- list("cigarette_availability" = NULL,
                 "src_scored" = NULL,
                 "cigarette_counts" = NULL,
                 "Y" = NULL)

this_outcome <- "cigarette_availability"
my_list2[[this_outcome]] <- c("age", "is_male", 
                              "income_val", "nd_mean", "food_security_mean",
                              "has_partner", "sni_count", "sni_active", "sni_people",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""), 
                              paste(this_outcome, "_mean_past24hrs", suffix, sep = ""))

this_outcome <- "src_scored"
my_list2[[this_outcome]] <- c("srq_mean", "se_social", "se_habit", "se_negaff",
                              "has_partner", "sni_count", "sni_active", "sni_people",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""), 
                              paste(this_outcome, "_mean_past24hrs", suffix, sep = ""),
                              paste("Y", "_lag1", suffix, sep = ""), 
                              paste("Y", "_sum_past24hrs", suffix, sep = ""))

this_outcome <- "cigarette_counts"
my_list2[[this_outcome]] <- c("age", "is_male", "income_val",
                              "baseline_tobacco_history", "Nicotine_dep",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""), 
                              paste(this_outcome, "_sum_past24hrs", suffix, sep = ""),
                              paste("Y", "_lag1", suffix, sep = ""), 
                              paste("Y", "_sum_past24hrs", suffix, sep = ""))

this_outcome <- "Y"
my_list2[[this_outcome]] <- c("age", "is_male", "income_val",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""), 
                              paste(this_outcome, "_sum_past24hrs", suffix, sep = ""),
                              paste(c("any_response_2qs", "any_app_usage_preblock", "total_app_usage_time_spent_preblock"), suffix, sep = ""))

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#    (This is a "smaller" set of possible predictors we will consider         #
#     in case fitting a model with a "larger" set of possible predictors      #
#     above does not converge)                                                #
#                                                                             #
###############################################################################
my_list3 <- list("cigarette_availability" = NULL,
                 "src_scored" = NULL,
                 "cigarette_counts" = NULL,
                 "Y" = NULL)

this_outcome <- "cigarette_availability"
my_list3[[this_outcome]] <- c("age", "is_male", 
                              "income_val", "nd_mean", "food_security_mean",
                              "has_partner", "sni_count", "sni_active", "sni_people",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""))

this_outcome <- "src_scored"
my_list3[[this_outcome]] <- c("srq_mean", "se_social", "se_habit", "se_negaff",
                              "has_partner", "sni_count", "sni_active", "sni_people",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""),
                              paste("Y", "_lag1", suffix, sep = ""))

this_outcome <- "cigarette_counts"
my_list3[[this_outcome]] <- c("age", "is_male", "income_val",
                              "baseline_tobacco_history", "Nicotine_dep",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""),
                              paste("Y", "_lag1", suffix, sep = ""))

this_outcome <- "Y"
my_list3[[this_outcome]] <- c("age", "is_male", "income_val",
                              paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""),
                              paste(this_outcome, "_lag1", suffix, sep = ""),
                              paste(c("any_response_2qs"), suffix, sep = ""))

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#    (This is a "smaller" set of possible predictors we will consider         #
#     in case fitting a model with a "larger" set of possible predictors      #
#     above does not converge)                                                #
#                                                                             #
###############################################################################
my_list4 <- list("cigarette_availability" = NULL,
                 "src_scored" = NULL,
                 "cigarette_counts" = NULL,
                 "Y" = NULL)

this_outcome <- "cigarette_availability"
my_list4[[this_outcome]] <- c(paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""))  # This model includes only the main effect for treatment

this_outcome <- "src_scored"
my_list4[[this_outcome]] <- c(paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""))  # This model includes only the main effect for treatment

this_outcome <- "cigarette_counts"
my_list4[[this_outcome]] <- c(paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""))  # This model includes only the main effect for treatment

this_outcome <- "Y"
my_list4[[this_outcome]] <- c(paste(c(this_outcome, "is_high_effort", "is_low_effort"), suffix, sep = ""))  # This model includes only the main effect for treatment

###############################################################################
#                                                                             #
#    Specify variables we would consider as predictors in imputation models   #
#    (This is a "smaller" set of possible predictors we will consider         #
#     in case fitting a model with a "larger" set of possible predictors      #
#     above does not converge)                                                #
#                                                                             #
###############################################################################
my_list5 <- list("cigarette_availability" = NULL,
                 "src_scored" = NULL,
                 "cigarette_counts" = NULL,
                 "Y" = NULL)

this_outcome <- "cigarette_availability"
my_list5[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""))  # This is an intercept-only model

this_outcome <- "src_scored"
my_list5[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""))  # This is an intercept-only model

this_outcome <- "cigarette_counts"
my_list5[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""))  # This is an intercept-only model

this_outcome <- "Y"
my_list5[[this_outcome]] <- c(paste(this_outcome, suffix, sep = ""))  # This is an intercept-only model

###############################################################################
#                                                                             #
#              Create a list in which to save any mice logged events          #
#                                                                             #
###############################################################################
list_mice_logged_events <- list("cigarette_availability" = NULL,
                                "src_scored" = NULL,
                                "cigarette_counts" = NULL,
                                "Y" = NULL)

list_mice_model <- list("cigarette_availability" = NULL,
                        "src_scored" = NULL,
                        "cigarette_counts" = NULL,
                        "Y" = NULL)

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome in stratum 3                #
#                                                                             #
###############################################################################

###############################################################################
# Step 1. Impute cigarette_availability
###############################################################################
this_outcome <- "cigarette_availability"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list3[[which(names(my_list3) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list4[[which(names(my_list4) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  used_intercept_only <- TRUE
  consider_these_vars <- my_list5[[which(names(my_list5) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  info_criterion <- extractAIC(fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

# Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
# This check will help capture those cases
result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
# This will cause execution of this script to stop at this point
# if the value of the argument is false
check_throw_error(check_convergence_result)

# Workflow for generating imputed values --------------------------------------
if((check_convergence_result == TRUE) & (used_intercept_only == FALSE)){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "backward",
            scope = list(lower = as.formula(paste("~", "is_high_effort", suffix, "+", "is_low_effort", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  for(i in 1:length(selected_vars)){
    pred_mat[LHS, selected_vars[i]] <- 1
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

###############################################################################
# Step 2. Impute src_scored
###############################################################################
this_outcome <- "src_scored"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list3[[which(names(my_list3) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list4[[which(names(my_list4) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  used_intercept_only <- TRUE
  consider_these_vars <- my_list5[[which(names(my_list5) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  info_criterion <- extractAIC(fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

# Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
# This check will help capture those cases
result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
# This will cause execution of this script to stop at this point
# if the value of the argument is false
check_throw_error(check_convergence_result)

# Workflow for generating imputed values --------------------------------------
if((check_convergence_result == TRUE) & (used_intercept_only == FALSE)){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "backward",
            scope = list(lower = as.formula(paste("~", "is_high_effort", suffix, "+", "is_low_effort", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  for(i in 1:length(selected_vars)){
    pred_mat[LHS, selected_vars[i]] <- 1
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

###############################################################################
# Step 3. Impute cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list3[[which(names(my_list3) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list4[[which(names(my_list4) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  used_intercept_only <- TRUE
  consider_these_vars <- my_list5[[which(names(my_list5) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  info_criterion <- extractAIC(fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

# Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
# This check will help capture those cases
result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
# This will cause execution of this script to stop at this point
# if the value of the argument is false
check_throw_error(check_convergence_result)

# Workflow for generating imputed values --------------------------------------
if((check_convergence_result == TRUE) & (used_intercept_only == FALSE)){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "backward",
            scope = list(lower = as.formula(paste("~", "is_high_effort", suffix, "+", "is_low_effort", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  for(i in 1:length(selected_vars)){
    pred_mat[LHS, selected_vars[i]] <- 1
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[LHS]] <- "pmm"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

###############################################################################
# Step 4. Y
###############################################################################
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))

if(class(rows_violate_restriction[[LHS]]) == "factor"){
  rows_violate_restriction[[LHS]] <- as.numeric(rows_violate_restriction[[LHS]]) # Do not need to subtract 1 since all of these will be NA's
}

list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
cond1 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 0", sep = ""), ")", sep = "")
cond2 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
cond3 <- paste("(", paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("any_recent_eligible_dp" , suffix, " == 1", sep = ""), ")", sep = "")
list_restriction_meet_string <- list("less_stringent" = list(cond1, cond2),
                                     "most_stringent" = cond3)

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 1
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_01_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_01_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for less stringent restriction -----------------------------------
curr_stratum <- 2
n_participants_within_stratum_current_dp <- n_participants_meet_sparse_restrictions_stratum_02_current_dp  # Note that this counts includes the replicates; so to obtain unique number of participants, divide this count by two
dat_imputed_stratum_current_dp <- dat_imputed_stratum_02_current_dp

if(n_participants_within_stratum_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]][[curr_stratum]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[LHS]])
  idx_no_impute <- cci(rows_meet_restriction[[LHS]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_imputed_stratum_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    
    if(class(rows_meet_restriction[[LHS]]) == "factor"){
      rows_meet_restriction[[LHS]] <- as.numeric(rows_meet_restriction[[LHS]]) - 1
    }
    
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

n_participants_remain <- rows_meet_restriction %>% filter(replicate_id == 0) %>% nrow(.)  # Need to just grab all the rows having replicate_id == 0 to count the total number of unique individuals
if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(rows_meet_restriction))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(rows_meet_restriction)
meth_list <- dummy_list

vars <- colnames(rows_meet_restriction)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list3[[which(names(my_list3) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list4[[which(names(my_list4) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

if(check_convergence_result == FALSE){
  used_intercept_only <- TRUE
  consider_these_vars <- my_list5[[which(names(my_list5) == this_outcome)]]
  dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  info_criterion <- extractAIC(fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
}else{
  # Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
  # This check will help capture those cases
  result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
  check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
}

# Note that fit$converged can be TRUE but some values for fit$coefficients might be NA's
# This check will help capture those cases
result_of_another_check <- (sum(is.na(fit$coefficients)) == 0)
check_convergence_result <- if_else(result_of_another_check == TRUE, TRUE, FALSE)
# This will cause execution of this script to stop at this point
# if the value of the argument is false
check_throw_error(check_convergence_result)

# Workflow for generating imputed values --------------------------------------
if((check_convergence_result == TRUE) & (used_intercept_only == FALSE)){
  stepwise_convergence <- TRUE
  
  fit_step <- tryCatch(expr = {
    stepAIC(fit, 
            direction = "backward",
            scope = list(lower = as.formula(paste("~", "is_high_effort", suffix, "+", "is_low_effort", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
            trace = FALSE, 
            k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)
  }, warning = function(w){"Hey, a warning"})
  
  stepwise_convergence <- (class(fit_step)[[1]] == "glm")
  
  if(stepwise_convergence == TRUE){
    use_fit <- fit_step
  }else{
    use_fit <- fit
  }
  
  info_criterion <- extractAIC(use_fit, k = use_penalty)[[2]]  # Calculated info criterion of selected model
  
  selected_vars <- names(use_fit$coefficients)
  selected_vars <- selected_vars[-1] # remove the intercept term because the predictorMatrix argument does not allow a row/column for that
  
  for(i in 1:length(selected_vars)){
    pred_mat[LHS, selected_vars[i]] <- 1
  }
  
  meth_list[[LHS]] <- "logreg"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1
  
  # Calculate AUC...
  fit_final <- glm(use_fit$formula, family = binomial, data = dat_for_variable_selection)
  observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[[LHS]]),LHS]
  estimated_roc <- roc(observed_vals, fitted.values(fit_final))
  estimated_auc <- as.numeric(estimated_roc$auc)
  
  print(estimated_auc)
  print(use_fit$formula)
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(rows_meet_restriction))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(rows_meet_restriction)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[LHS]] <- "logreg"
  imp <- mice(data = rows_meet_restriction, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
  rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1
  
  # Calculate AUC...
  fit_final <- glm(fit$formula, family = binomial, data = dat_for_variable_selection)
  observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[[LHS]]),LHS]
  estimated_roc <- roc(observed_vals, fitted.values(fit_final))
  estimated_auc <- as.numeric(estimated_roc$auc)
  
  print(estimated_auc)
  print(fit$formula)
}

# Before we move on to the next variable...
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[LHS]])

###############################################################################
# Save
###############################################################################
saveRDS(list_mice_logged_events, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_events_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
saveRDS(list_mice_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imputation_model_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
saveRDS(estimated_auc, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("estimated_auc_stratum", st_num, "_dp", current_dp_value, ".rds", sep = "")))
saveRDS(dat_wide, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num,  paste("dat_wide_completed", "_dp", current_dp_value, ".rds", sep = "")))  # Contains imputed data for all strata up to the current decision point

