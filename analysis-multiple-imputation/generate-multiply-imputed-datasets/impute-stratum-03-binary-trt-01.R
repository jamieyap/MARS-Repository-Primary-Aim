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

# Read in completed dataset from previous time-point
dat_imputed_stratum_01 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum_01.rds"))
dat_imputed_stratum_02 <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum_02.rds"))
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

# Initialize lists which will store imputation method and formula -------------
imp0 <- mice(data = rows_meet_restriction, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
meth_list <- lapply(meth_list, function(x){return("")})
formula_list <- imp0$formulas

# Workflow for variable selection --------------------------------------------------
new_time_varying_vars_to_consider1 <- paste(c(this_outcome, "coinflip"), suffix, sep = "")
new_time_varying_vars_to_consider2 <- c(paste(this_outcome, "_lag1", suffix, sep = ""))
new_time_varying_vars_to_consider3 <- paste(c("any_response_2qs"), suffix, sep = "")
new_baseline_vars_to_consider <- c("age", "is_male", "income_val")
consider_these_vars <- c(new_baseline_vars_to_consider, new_time_varying_vars_to_consider1, new_time_varying_vars_to_consider2, new_time_varying_vars_to_consider3)

dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower= as.formula(paste("~", "coinflip", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
previous_var <- LHS

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

# Initialize lists which will store imputation method and formula -------------
imp0 <- mice(data = rows_meet_restriction, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
meth_list <- lapply(meth_list, function(x){return("")})
formula_list <- imp0$formulas

# Workflow for variable selection --------------------------------------------------
new_time_varying_vars_to_consider1 <- paste(c(this_outcome, "coinflip"), suffix, sep = "")
new_time_varying_vars_to_consider2 <- c(paste(this_outcome, "_lag1", suffix, sep = ""))
new_time_varying_vars_to_consider3 <- paste(c("any_response_2qs"), suffix, sep = "")
new_baseline_vars_to_consider <- c("age", "is_male", "income_val")
consider_these_vars <- c(previous_var, new_baseline_vars_to_consider, new_time_varying_vars_to_consider1, new_time_varying_vars_to_consider2, new_time_varying_vars_to_consider3)

dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower= as.formula(paste("~", "coinflip", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
previous_var <- c(previous_var, LHS)

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

# Initialize lists which will store imputation method and formula -------------
imp0 <- mice(data = rows_meet_restriction, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
meth_list <- lapply(meth_list, function(x){return("")})
formula_list <- imp0$formulas

# Workflow for variable selection --------------------------------------------------
new_time_varying_vars_to_consider1 <- paste(c(this_outcome, "coinflip"), suffix, sep = "")
new_time_varying_vars_to_consider2 <- c(paste(this_outcome, "_lag1", suffix, sep = ""))
new_time_varying_vars_to_consider3 <- paste(c("any_response_2qs"), suffix, sep = "")
new_baseline_vars_to_consider <- c("age", "is_male", "income_val")
consider_these_vars <- c(previous_var, new_baseline_vars_to_consider, new_time_varying_vars_to_consider1, new_time_varying_vars_to_consider2, new_time_varying_vars_to_consider3)

dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower= as.formula(paste("~", "coinflip", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
previous_var <- c(previous_var, LHS)

###############################################################################
# Step 4. Y
###############################################################################
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
rows_violate_restriction[[LHS]] <- as.numeric(rows_violate_restriction[[LHS]]) - 1
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

# Initialize lists which will store imputation method and formula -------------
imp0 <- mice(data = rows_meet_restriction, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
meth_list <- lapply(meth_list, function(x){return("")})
formula_list <- imp0$formulas

# Workflow for variable selection --------------------------------------------------
new_time_varying_vars_to_consider1 <- paste(c(this_outcome, "coinflip"), suffix, sep = "")
new_time_varying_vars_to_consider2 <- c(paste(this_outcome, "_lag1", suffix, sep = ""))
new_time_varying_vars_to_consider3 <- paste(c("any_response_2qs"), suffix, sep = "")
new_baseline_vars_to_consider <- c("age", "is_male", "income_val")

consider_these_vars <- c(new_time_varying_vars_to_consider1, new_time_varying_vars_to_consider2, new_time_varying_vars_to_consider3, new_baseline_vars_to_consider)
dat_for_variable_selection <- rows_meet_restriction %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- glm(as.formula(paste(LHS, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)

fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower= as.formula(paste("~", "coinflip", suffix, sep = ""))), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "logreg"
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
rows_meet_restriction_completed <- complete(imp, 1)  # Update rows_meet_restriction
rows_meet_restriction_completed[[LHS]] <- as.numeric(rows_meet_restriction_completed[[LHS]]) - 1

list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
dat_wide <- bind_rows(list_collect_data)
previous_var <- c(previous_var, LHS)

# Calculate AUC
fit_final <- glm(fit_step$formula, family = binomial, data = dat_for_variable_selection)
observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[[LHS]]),LHS]
estimated_roc <- roc(observed_vals, fitted.values(fit_final))
estimated_auc <- as.numeric(estimated_roc$auc)

print(estimated_auc)
print(fit_step$formula)

###############################################################################
# Save
###############################################################################
saveRDS(estimated_auc, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("estimated_auc_dp", current_dp_value, ".rds", sep = "")))
saveRDS(dat_wide, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num,  paste("dat_wide_completed_dp", current_dp_value, ".rds", sep = "")))


