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
suffix_lag1 <- paste("_dp" ,  current_dp_value - 1, sep = "")

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

dat_timevarying_wide_current_dp <- dat_timevarying_wide %>% select(replicate_id, participant_id, ends_with(suffix))
dat_wide <- left_join(x = dat_wide_from_prior_step, y = dat_timevarying_wide_current_dp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
dat_wide_init <- dat_wide

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
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction -----------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()
this_outcome <- "quick_survey_response"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste("ffmq_nonjudge", "income_val",
             paste("quick_survey_nreported_past24hrs", suffix, sep = ""), 
             paste("quick_survey_response_lag1", suffix, sep = ""), 
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 1,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", "quick_survey_response", suffix, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
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
# self-regulatory strategies) and other variables assessed in EMA
###############################################################################
dat_wide <- dat_wide_completed

list_collect_data <- list()

# Rows where imputation at the current decision point will NOT be performed ----
restriction_violate_string <- paste("eligibility", suffix, " == 0", sep = "")
rows_violate_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_violate_string))
list_collect_data <- append(list_collect_data, list(rows_violate_restriction))

# Rows where imputation at the current decision point will be performed -------
list_restriction_meet_string <- list("most_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 1", sep = ""), sep = ""),
                                     "less_stringent" = paste(paste("eligibility", suffix, " == 1", sep = ""), " & ", paste("eligibility_lag1" , suffix, " == 0", sep = ""), sep = ""))

# Imputation for most stringent restriction ------------------------------------
restriction_meet_string <- list_restriction_meet_string[["most_stringent"]]
rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))

my_list <- list()

this_outcome <- "Y"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("Y_lag1", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("cigarette_counts", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

this_outcome <- "cigarette_counts"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("is_high_effort", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

this_outcome <- "src_scored"
LHS <- paste(this_outcome, suffix, sep = "")
RHS <- paste(paste("src_scored_lag1", suffix, sep = ""),
             paste("is_high_effort", suffix, sep = ""),
             paste("is_low_effort", suffix, sep = ""),
             paste("cigarette_counts", suffix, sep = ""),
             sep = " + ")
my_list[[LHS]] <- as.formula(paste(LHS, RHS, sep = " ~ "))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = use_maxit_value,
            formulas =  my_list)

rows_meet_restriction_completed <- complete(imp, 1) 
list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imp_obj_", "ema_response", suffix, ".rds", sep = "")))

# Imputation for less stringent restriction -----------------------------------
if(n_participants_meet_sparse_restrictions_current_dp > 0){
  restriction_meet_string <- list_restriction_meet_string[["less_stringent"]]
  rows_meet_restriction <- dat_wide %>% filter(!!rlang::parse_expr(restriction_meet_string))
  list_collect_ema_varname_imputed <- list()
  
  # -- engagement in self-regulatory strategies -------------------------------
  this_outcome <- "Y"
  LHS <- paste(this_outcome, suffix, sep = "")
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
  }
  
  list_collect_ema_varname_imputed <- append(list_collect_ema_varname_imputed, list(this_outcome))
  # -- cigarette counts -------------------------------------------------------
  this_outcome <- "cigarette_counts"
  LHS <- paste(this_outcome, suffix, sep = "")
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
  }
  
  list_collect_ema_varname_imputed <- append(list_collect_ema_varname_imputed, list(this_outcome))
  # -- self-regulatory capacity -----------------------------------------------
  this_outcome <- "src_scored"
  LHS <- paste(this_outcome, suffix, sep = "")
  idx_impute <- ici(rows_meet_restriction[[paste(LHS, sep = "")]])
  idx_no_impute <- cci(rows_meet_restriction[[paste(LHS, sep = "")]])
  n_to_impute <- sum(idx_impute)
  n_to_not_impute <- sum(idx_no_impute)
  
  if(n_to_impute > 0){
    tmp <- dat_completed_sparse_restrictions_current_dp %>% select(replicate_id, participant_id, all_of(this_outcome))
    rows_meet_restriction <- left_join(x = rows_meet_restriction, y = tmp, by = join_by(replicate_id == replicate_id, participant_id == participant_id))
    rows_meet_restriction[[LHS]] <- if_else(is.na(rows_meet_restriction[[LHS]]), rows_meet_restriction[[this_outcome]], rows_meet_restriction[[LHS]])
  }
  
  list_collect_ema_varname_imputed <- append(list_collect_ema_varname_imputed, list(this_outcome))
  # -- final data processing step before exiting ------------------------------
  collect_ema_varname_imputed <- unlist(list_collect_ema_varname_imputed)
  rows_meet_restriction_completed <- rows_meet_restriction %>% select(-any_of(collect_ema_varname_imputed))
  list_collect_data <- append(list_collect_data, list(rows_meet_restriction_completed))
}

# Wrap up ---------------------------------------------------------------------
dat_wide_completed <- bind_rows(list_collect_data)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

###############################################################################
# Save
###############################################################################
saveRDS(dat_wide_init, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_init_block", current_dp_value, ".rds", sep = "")))
saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_wide_completed_block", current_dp_value, ".rds", sep = "")))


