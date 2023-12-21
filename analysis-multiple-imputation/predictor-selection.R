###############################################################################
# Load packages and datasets
###############################################################################
rm(list = ls())
mi_dataset_num <- 1
source("paths.R")
library(tidyverse)
library(ROCR)
library(cgAUC)

dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))
dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with("_dp1"))

dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
dat_wide_subset <- dat_wide %>% filter(eligibility_dp1 == 1) %>% mutate(log_cigarette_counts_dp1 = log(cigarette_counts_dp1 + 1))

fit <- glm(quick_survey_response_dp1 ~ srq_mean, data = dat_wide_subset, family = binomial)
summary(fit)

fit <- glm(Y_dp1 ~ is_low_effort_dp1 + is_high_effort_dp1 + gratitude, data = dat_wide_subset, family = binomial)
summary(fit)

fit <- glm(log_cigarette_counts_dp1 ~ is_low_effort_dp1 + is_high_effort_dp1 + baseline_tobacco_history + ffmq_nonjudge, data = dat_wide_subset, family = gaussian)
summary(fit)

###############################################################################
# Load packages and datasets
###############################################################################
rm(list = ls())
mi_dataset_num <- 1
current_dp_value <- 7
suffix <- paste("_dp" ,  current_dp_value, sep = "")
suffix_lag1 <- paste("_dp" ,  current_dp_value - 1, sep = "")
source("paths.R")
library(tidyverse)
library(mice)

this_data_file <- paste("dat_wide_completed_block", current_dp_value - 1, ".rds", sep = "")
dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, this_data_file))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with(suffix))
dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
dat_wide[[paste("log_cigarette_counts", suffix, sep = "")]] <- log(dat_wide[[paste("cigarette_counts", suffix, sep = "")]] + 1)

###############################################################################
# Create new variables
###############################################################################

################################################################################
# How many replicates do we have?
################################################################################
all_replicate_ids <- unique(dat_wide[["replicate_id"]])
maximum_replicate_id <- max(all_replicate_ids)
minimum_replicate_id <- min(all_replicate_ids)

###############################################################################
# Step 0. Update completed dataset -- eligibility for micro-randomization
# at the prior decision point
###############################################################################
dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] <- dat_wide[[paste("eligibility", suffix_lag1, sep = "")]]

###############################################################################
# Step 0. Update completed dataset -- two-question survey response 
# at the prior decision point
###############################################################################
dat_wide[[paste("quick_survey_response_lag1", suffix, sep = "")]] <- as.numeric(dat_wide[[paste("quick_survey_response", suffix_lag1, sep = "")]]) - 1
dat_wide[[paste("quick_survey_response_lag1", suffix, sep = "")]] <- if_else(dat_wide[[paste("eligibility_lag1", suffix, sep = "")]] == 0, -1, dat_wide[[paste("quick_survey_response_lag1", suffix, sep = "")]])

###############################################################################
# Step 0. Update completed dataset -- Y at the prior decision point
###############################################################################
dat_wide[[paste("Y_lag1", suffix, sep = "")]] <- as.numeric(dat_wide[[paste("Y", suffix_lag1, sep = "")]]) - 1
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
      
      # Recall that the original variable is a factor with two levels: 0 and 1
      # Thus, when applying as.numeric() the resulting variable takes on values 1 and 2
      # So, we subtract by 1 so that the resulting variable takes on values of 0 and 1
      dat_current[[variable_name_transformed]] <- as.numeric(dat_current[[variable_name_original]]) - 1
      
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
# Step 0. Update completed dataset -- mean of self-regulatory capacity
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
          dat_current[this_participant, variable_name_past24hrs] <- resulting_value/dat_current[this_participant, paste("counts_rand_past24hrs", suffix, sep = "")]
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
        dat_wide[this_participant, variable_name_past24hrs] <- resulting_value/dat_wide[this_participant, paste("counts_rand_past24hrs", suffix, sep = "")]
      }
    }
  } 
}

###############################################################################
# Step 0. Update completed dataset -- sum of dichotomized response to 2qs in 
# past 24 hours
###############################################################################
this_variable <- "quick_survey_response"
this_indicator <- "eligibility"

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

###############################################################################
# Logistic/linear regression
###############################################################################

restriction_meet_string <- paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 1", sep = ""))
dat_wide_subset <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

# Model 1
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("Y_lag1", suffix, sep = ""),
             sep = " + ")
fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = binomial)
summary(fit)

# Model 2
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("Y_sum_past24hrs", suffix, sep = ""),
             sep = " + ")
fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = binomial)
summary(fit)

# Model 3
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("Y_sum_past24hrs", suffix, sep = ""),
             "baseline_tobacco_history",
             sep = " + ")
fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = binomial)
summary(fit)

# Model 4
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("Y_sum_past24hrs", suffix, sep = ""),
             "srq_mean",
             sep = " + ")
fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = binomial)
summary(fit)

# Model 5
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("Y_sum_past24hrs", suffix, sep = ""),
             "srq_mean",
             paste("Y_lag1", suffix, sep = ""),
             sep = " + ")
fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = binomial)
summary(fit)

# Model 1
this_outcome <- "log_cigarette_counts"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge",
             "srq_mean",
             "baseline_tobacco_history",
             "income_val",
             paste("Y_nreported_past24hrs", suffix, sep = ""),
             paste("cigarette_counts_lag1", suffix, sep = ""),
             paste("cigarette_counts_sum_past24hrs", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             sep = " + ")

fit <- glm(as.formula(paste(LHS, RHS, sep = " ~ ")), data = dat_wide_subset, family = gaussian)
summary(fit)

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "predictor-selection"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "predictor-selection"))
}

write.csv(round(summary(fit)[["coefficients"]], 3), file.path(path_multiple_imputation_pipeline_data, "predictor-selection", paste("m1_imputed_", mi_dataset_num, ".csv", sep = "")))

