###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters for generating the current completed dataset
###############################################################################
mi_dataset_num <- .__par_mi_number
use_maxit_value <- .__par_maxit_value
current_dp_value <- .__par_decision_point_now
suffix <- paste("_dp" ,  current_dp_value, sep = "")

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

# -----------------------------------------------------------------------------
# This is for the case when eligible at the current decision point but not
# eligible at any time in the past 24 hours
# -----------------------------------------------------------------------------
dat_completed_sparse_restrictions <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_completed_sparse_restrictions.rds"))
dat_completed_sparse_restrictions_current_dp <- dat_completed_sparse_restrictions %>% filter(decision_point == current_dp_value)
n_participants_meet_sparse_restrictions_current_dp <- nrow(dat_completed_sparse_restrictions_current_dp)

# -----------------------------------------------------------------------------
# This is for the other cases
# -----------------------------------------------------------------------------
this_data_file <- paste("dat_wide_completed_block", current_dp_value - 1, ".rds", sep = "")
dat_wide_from_prior_step <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, this_data_file))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_timevarying_wide.rds"))

dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(participant_id, ends_with(suffix))
dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(participant_id == participant_id))
dat_wide_init <- dat_wide

###############################################################################
# Step 0. Update completed dataset -- most recent eligible decision point
###############################################################################
variable_name_relevance_indicator <- paste("any_recent_eligible_dp", suffix, sep = "")
variable_name_matched_decision_point <- paste("matched_recent", suffix, sep = "")
variable_name_new <- paste("engagement_most_recent_eligible", suffix, sep = "")

for(this_participant in 1:nrow(dat_wide)){
  if(dat_wide[this_participant, variable_name_relevance_indicator] == 1){
    matched_dp <- dat_wide[this_participant, variable_name_matched_decision_point]
    matched_value <- dat_wide[this_participant, paste("Y_dp", matched_dp, sep = "")]
    matched_value <- as.numeric(matched_value) - 1
    dat_wide[this_participant, variable_name_new] <- matched_value
  }
}

###############################################################################
# Step 0. Update completed dataset -- sum of Y in past 24 hours
###############################################################################
this_variable <- "Y"
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
# Step 0. Update completed dataset -- sum of cigarette counts in past 24 hours
###############################################################################
this_variable <- "cigarette_counts"
this_indicator <- "eligibility"

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

###############################################################################
# Step 0. Update completed dataset -- mean of self-regulatory capacity
# in past 24 hours
###############################################################################
this_variable <- "src_scored"
this_indicator <- "eligibility"

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
# Step 1. Impute responses to 2qs
#
# Specify a regression model for each variable individually.
# Since we only wish to impute responses to 2qs at this step, our list of 
# formulas only consist of the regression model for response to 2qs
# Other variables have missing values but since we did not specify a model 
# for them at this step, the mice package will not impute them
###############################################################################

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ---
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()
this_outcome <- "quick_survey_response"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge", "income_val",
             paste("quick_survey_nreported_past24hrs", suffix, sep = ""), 
             paste("quick_survey_response_sum_past24hrs", suffix, sep = ""), 
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", LHS, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Wrap up before moving on to the next variable -------------------------------
dat_wide_completed <- bind_rows(list_collect_data)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

###############################################################################
# Step 2. Impute primary proximal outcome (engagement in 
# self-regulatory strategies)
###############################################################################
dat_wide <- dat_wide_completed

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ----
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed --------
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction ------------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()
this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge",
             "srq_mean",
             paste("src_scored_mean_past24hrs", suffix, sep = ""),
             paste("Y_nreported_past24hrs", suffix, sep = ""), 
             paste("Y_sum_past24hrs", suffix, sep = ""),
             paste("engagement_most_recent_eligible", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", LHS, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Wrap up before moving on to the next variable -------------------------------
dat_wide_completed <- bind_rows(list_collect_data)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

###############################################################################
# Step 2. Impute cigarette counts
###############################################################################
dat_wide <- dat_wide_completed

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ----
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed --------
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction ------------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()
this_outcome <- "cigarette_counts"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge",
             "srq_mean",
             paste("Y_nreported_past24hrs", suffix, sep = ""), 
             paste("Y", suffix, sep = ""), 
             paste("cigarette_counts_sum_past24hrs", suffix, sep = ""),
             paste("engagement_most_recent_eligible", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", LHS, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Wrap up before moving on to the next variable -------------------------------
dat_wide_completed <- bind_rows(list_collect_data)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

###############################################################################
# Step 3. Impute self-regulatory capacity
###############################################################################
dat_wide <- dat_wide_completed

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ----
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed --------
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("elig24hrs" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction ------------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()
this_outcome <- "src_scored"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge",
             "srq_mean",
             paste("Y_nreported_past24hrs", suffix, sep = ""), 
             paste("Y", suffix, sep = ""), 
             paste("cigarette_counts", suffix, sep = ""),
             paste("src_scored_mean_past24hrs", suffix, sep = ""), 
             paste("engagement_most_recent_eligible", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", LHS, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
    rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(this_outcome))
    list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
  }
}

# Wrap up before moving on to the next variable -------------------------------
dat_wide_completed <- bind_rows(list_collect_data)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

###############################################################################
# Save
###############################################################################
saveRDS(dat_wide_init, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_init_block", current_dp_value, ".rds", sep = "")))
saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_completed_block", current_dp_value, ".rds", sep = "")))


