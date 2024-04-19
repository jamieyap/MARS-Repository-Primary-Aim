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
which_penalty <- "AIC"  # Can be set to either "AIC" or "BIC"

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
dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))

# Lay out all of the options
cond1 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0)"  # -- stratum 1
cond2 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1)"  # -- stratum 2
cond3 <- "(eligibility == 1 & eligibility_lag1 == 1)"                                # -- stratum 3

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome in stratum 2                #
#                                                                             #
###############################################################################

# Recall that dat_primary_aim_replicated.rds is an output of the script create-replicated-dataset.R
dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))

new_time_varying_vars_to_consider <- c("days_between_v1_and_coinflip_local", "driving_data_not_found_combined", "hours_elapsed_since_most_recent_eligible")

keep_these_cols <- c("replicate_id", "participant_id", "decision_point",
                     "is_high_effort", "is_low_effort",
                     "eligibility", "eligibility_lag1", "any_recent_eligible_dp",
                     "Y", "cigarette_counts", "src_scored", "cigarette_availability",
                     "any_response_2qs", "hour_coinflip_local", new_time_varying_vars_to_consider)

dat_long <- dat_long %>% select(all_of(keep_these_cols))
dat_long_merged <- left_join(x = dat_wide_completed_baseline, y = dat_long, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

new_baseline_vars_to_consider <- c("FinancialStrain", "nd_mean", "food_security_mean", "SSSladders", "pp1_1",
                                    "baseline_tobacco_history", "Nicotine_dep",
                                    "srq_mean", "se_social", "se_habit", "se_negaff",
                                    "has_partner", "sni_count", "sni_active", "sni_people", "isel_belonging", "isel_appraisal", "isel_tangible")
new_vars_to_consider <- c(new_time_varying_vars_to_consider, new_baseline_vars_to_consider)

###############################################################################
# Specify relevant restriction
###############################################################################
dat_stratum <- dat_long_merged %>% filter(!!rlang::parse_expr(cond2))
n_participants_remain <- dat_stratum %>% filter(replicate_id == 0) %>% nrow(.)

if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
}

###############################################################################
# Initialize lists which will store imputation method and formula
###############################################################################
imp0 <- mice(data = dat_stratum, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
meth_list <- lapply(meth_list, function(x){return("")})
formula_list <- imp0$formulas

###############################################################################
# Workflow for cigarette_availability
###############################################################################
LHS <- "cigarette_availability"

consider_these_vars <- c(LHS,
                         "is_high_effort", "is_low_effort",
                         "age", "is_male", "income_val",
                         "any_response_2qs", "hour_coinflip_local",
                         new_vars_to_consider)

dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ . + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local)", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower=~is_high_effort + is_low_effort), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = dat_stratum, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
dat_stratum <- complete(imp, 1)  # Update dat_stratum
previous_var <- LHS

###############################################################################
# Workflow for src_scored
###############################################################################
LHS <- "src_scored"

consider_these_vars <- c(LHS, previous_var,
                         "is_high_effort", "is_low_effort",
                         "age", "is_male", "income_val",
                         "any_response_2qs", "hour_coinflip_local",
                         new_vars_to_consider)

dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ . + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local)", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower=~is_high_effort + is_low_effort), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = dat_stratum, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
dat_stratum <- complete(imp, 1)  # Update dat_stratum
previous_var <- c(previous_var, LHS)

###############################################################################
# Workflow for cigarette_counts
###############################################################################
LHS <- "cigarette_counts"

consider_these_vars <- c(LHS, previous_var,
                         "is_high_effort", "is_low_effort",
                         "age", "is_male", "income_val",
                         "any_response_2qs", "hour_coinflip_local",
                         new_vars_to_consider)

dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ . + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local)", sep = "")), family = gaussian, data = dat_for_variable_selection)
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower=~is_high_effort + is_low_effort), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "pmm"
imp <- mice(data = dat_stratum, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)

# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
dat_stratum <- complete(imp, 1)  # Update dat_stratum
previous_var <- c(previous_var, LHS)

###############################################################################
# Workflow for Y
###############################################################################
LHS <- "Y"

consider_these_vars <- c(LHS, previous_var,
                         "is_high_effort", "is_low_effort",
                         "age", "is_male", "income_val",
                         "any_response_2qs", "hour_coinflip_local",
                         new_vars_to_consider)

dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))

fit <- glm(as.formula(paste(LHS, "~ . + I(days_between_v1_and_coinflip_local * days_between_v1_and_coinflip_local)", sep = "")), family = binomial, data = dat_for_variable_selection, control = list(maxit = 50))
fit_step <- stepAIC(fit, 
                    direction = "both",
                    scope = list(lower=~is_high_effort + is_low_effort), # The minimal model should have the main effect of the treatment indicators
                    trace = FALSE, 
                    k = use_penalty)  # To use AUC, set k=2. To use BIC, set k = log(n)

formula_list[[LHS]] <- fit_step$formula
meth_list[[LHS]] <- "logreg"
dat_stratum[[LHS]] <- as_factor(dat_stratum[[LHS]])
imp <- mice(data = dat_stratum, 
            m = 1, 
            maxit = use_maxit_value,
            meth =  meth_list,
            formulas = formula_list)


# Before we move on to the next variable...
meth_list[[LHS]] <- ""  # Reset meth
dat_stratum <- complete(imp, 1)  # Update dat_stratum
previous_var <- c(previous_var, LHS)
dat_stratum[[LHS]] <- as.numeric(dat_stratum[[LHS]]) - 1  # Convert back to numeric type

# Calculate AUC
fit_final <- glm(fit_step$formula, family = binomial, data = dat_for_variable_selection)
observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[["Y"]]),"Y"]
estimated_roc <- roc(observed_vals, fitted.values(fit_final))
estimated_auc <- as.numeric(estimated_roc$auc)

print(estimated_auc)
print(fit_step$formula)

###############################################################################
# Save
###############################################################################
saveRDS(estimated_auc, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "estimated_auc_stratum_02.rds"))
saveRDS(fit_step$formula, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "best_outcome_model_stratum_02.rds"))
saveRDS(imp$loggedEvents, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "logged_events_outcome_model_stratum_02.rds"))
saveRDS(dat_stratum, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_imputed_stratum_02.rds"))

