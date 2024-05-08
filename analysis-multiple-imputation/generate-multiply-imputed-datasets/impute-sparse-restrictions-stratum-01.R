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
st_num <- 1 # The stratum being imputed
which_penalty <- "BIC"  # Can be set to either "AIC" or "BIC"

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

# Read in completed dataset from previous time-point
dat_wide_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
these_cols_baseline <- colnames(dat_wide_completed_baseline)
these_cols_baseline <- these_cols_baseline[!(these_cols_baseline %in% c("replicate_id", "participant_id"))]

# Lay out all of the options
cond1 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0)"  # -- stratum 1
cond2 <- "(eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1)"  # -- stratum 2
cond3 <- "(eligibility == 1 & eligibility_lag1 == 1)"                                # -- stratum 3

use_cond <- case_when(
  st_num == 1 ~ cond1,
  st_num == 2 ~ cond2,
  st_num == 3 ~ cond3,
  .default = NA_character_
)

###############################################################################
#                                                                             #
#                 Impute missing proximal outcome in stratum 1                #
#                                                                             #
###############################################################################

# Recall that dat_primary_aim_replicated.rds is an 
# output of the script create-replicated-dataset.R
dat_long <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))
dat_long <- dat_long %>% select(-any_of(these_cols_baseline))
dat_long_merged <- left_join(x = dat_wide_completed_baseline, y = dat_long, by = join_by(replicate_id == replicate_id, participant_id == participant_id))

###############################################################################
#                                                                             #
#                          Specify relevant restriction                       #
#                                                                             #
###############################################################################
dat_stratum <- dat_long_merged %>% filter(!!rlang::parse_expr(use_cond))
dat_stratum[["Y"]] <- as_factor(dat_stratum[["Y"]])

###############################################################################
#                                                                             #
#                       Specify model selection criteria                      #
#                                                                             #
###############################################################################
n_participants_remain <- dat_stratum %>% filter(replicate_id == 0) %>% nrow(.)

if(which_penalty == "BIC"){
  use_penalty <- log(n_participants_remain)
}else if(which_penalty == "AIC"){
  use_penalty <- 2
}else{
  print("Choose valid option")
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
                             "income_val", "FinancialStrain", "nd_mean", "food_security_mean",
                             "has_partner", "sni_count", "sni_active", "sni_people",
                             this_outcome, "is_high_effort", "is_low_effort",
                             "hour_coinflip_local")

this_outcome <- "src_scored"
my_list[[this_outcome]] <- c("srq_mean", "se_social", "se_habit", "se_negaff",
                             "has_partner", "sni_count", "sni_active", "sni_people",
                             this_outcome, "is_high_effort", "is_low_effort",
                             "hour_coinflip_local",
                             "cigarette_availability")

this_outcome <- "cigarette_counts"
my_list[[this_outcome]] <- c("age", "is_male", 
                             "income_val", "FinancialStrain","nd_mean", "food_security_mean",
                             "baseline_tobacco_history",
                             "has_partner", "sni_count", "sni_active", "sni_people",
                             this_outcome, "is_high_effort", "is_low_effort",
                             "hour_coinflip_local",
                             "cigarette_availability", "src_scored")

this_outcome <- "Y"
my_list[[this_outcome]] <- c("age", "is_male", 
                             "income_val", "FinancialStrain","nd_mean", "food_security_mean",
                             this_outcome, "is_high_effort", "is_low_effort",
                             "any_response_2qs",
                             "hour_coinflip_local",
                             "cigarette_availability", "src_scored", "cigarette_counts")

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
my_list2[[this_outcome]] <- c("income_val", "FinancialStrain", "nd_mean", "food_security_mean",
                              this_outcome, "is_high_effort", "is_low_effort",
                              "hour_coinflip_local")

this_outcome <- "src_scored"
my_list2[[this_outcome]] <- c("has_partner", "sni_count", "sni_active", "sni_people",
                              this_outcome, "is_high_effort", "is_low_effort",
                              "hour_coinflip_local")

this_outcome <- "cigarette_counts"
my_list2[[this_outcome]] <- c("income_val", "FinancialStrain","nd_mean", "food_security_mean",
                              "baseline_tobacco_history",
                              "has_partner", "sni_count", "sni_active", "sni_people",
                              this_outcome, "is_high_effort", "is_low_effort",
                              "hour_coinflip_local")

this_outcome <- "Y"
my_list2[[this_outcome]] <- c("age", "is_male", 
                              "income_val", "FinancialStrain","nd_mean", "food_security_mean",
                              "is_complete_v1_quest",
                              this_outcome, "is_high_effort", "is_low_effort",
                              "any_response_2qs",
                              "hour_coinflip_local")

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
my_list3[[this_outcome]] <- c("income_val", "FinancialStrain","nd_mean", "food_security_mean",
                              this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "src_scored"
my_list3[[this_outcome]] <- c("has_partner", "sni_count", "sni_active", "sni_people",
                              this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "cigarette_counts"
my_list3[[this_outcome]] <- c("income_val", "FinancialStrain","nd_mean", "food_security_mean",
                              "baseline_tobacco_history",
                              this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "Y"
my_list3[[this_outcome]] <- c("age", "is_male", "income_val",
                              this_outcome, "is_high_effort", "is_low_effort",
                              "any_response_2qs")

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
my_list4[[this_outcome]] <- c(this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "src_scored"
my_list4[[this_outcome]] <- c(this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "cigarette_counts"
my_list4[[this_outcome]] <- c(this_outcome, "is_high_effort", "is_low_effort")

this_outcome <- "Y"
my_list4[[this_outcome]] <- c(this_outcome, "is_high_effort", "is_low_effort")

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
my_list5[[this_outcome]] <- c(this_outcome)

this_outcome <- "src_scored"
my_list5[[this_outcome]] <- c(this_outcome)

this_outcome <- "cigarette_counts"
my_list5[[this_outcome]] <- c(this_outcome)

this_outcome <- "Y"
my_list5[[this_outcome]] <- c(this_outcome)

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
# Step 1. Impute cigarette_availability
###############################################################################
this_outcome <- "cigarette_availability"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
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
            scope = list(lower = as.formula(paste("~", "is_high_effort", "+", "is_low_effort", sep = ""))), # The minimal model should have the main effect of the treatment indicators
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
    pred_mat[this_outcome, selected_vars[i]] <- 1
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

###############################################################################
# Step 2. Impute src_scored
###############################################################################
this_outcome <- "src_scored"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
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
            scope = list(lower = as.formula(paste("~", "is_high_effort", "+", "is_low_effort", sep = ""))), # The minimal model should have the main effect of the treatment indicators
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
    pred_mat[this_outcome, selected_vars[i]] <- 1
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

###############################################################################
# Step 3. Impute cigarette_counts
###############################################################################
this_outcome <- "cigarette_counts"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = gaussian, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
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
            scope = list(lower = as.formula(paste("~", "is_high_effort", "+", "is_low_effort", sep = ""))), # The minimal model should have the main effect of the treatment indicators
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
    pred_mat[this_outcome, selected_vars[i]] <- 1
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[this_outcome]] <- "pmm"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
}

# Before we move on to the next variable...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

###############################################################################
# Step 4. Y
###############################################################################
this_outcome <- "Y"

# Initialize list and matrix which will store imputation method and formula ---
dummy_list <- as.list(colnames(dat_stratum))
dummy_list <- lapply(dummy_list, function(x){return("")})
names(dummy_list) <- colnames(dat_stratum)
meth_list <- dummy_list

vars <- colnames(dat_stratum)
pred_mat <- matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))

# Workflow for variable selection ---------------------------------------------
used_intercept_only <- FALSE  # This is a flag for whether we used an intercept-only model for imputation
check_convergence_result <- TRUE

consider_these_vars <- my_list[[which(names(my_list) == this_outcome)]]
dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                warning = function(w){"Hey, a warning"})
check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue

if(check_convergence_result == FALSE){
  consider_these_vars <- my_list2[[which(names(my_list2) == this_outcome)]]
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
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
  dat_for_variable_selection <- dat_stratum %>% filter(replicate_id == 0) %>% select(all_of(consider_these_vars))
  fit <- tryCatch(expr = {glm(as.formula(paste(this_outcome, "~ .", sep = "")), family = binomial, data = dat_for_variable_selection)}, 
                  warning = function(w){"Hey, a warning"})
  check_convergence_result <- (class(fit)[[1]] == "glm")  # fit will be of class "character" if there was a convergence issue
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
            scope = list(lower = as.formula(paste("~", "is_high_effort", "+", "is_low_effort", sep = ""))), # The minimal model should have the main effect of the treatment indicators
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
    pred_mat[this_outcome, selected_vars[i]] <- 1
  }
  
  meth_list[[this_outcome]] <- "logreg"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              predictorMatrix = pred_mat)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  dat_stratum_completed[[this_outcome]] <- as.numeric(dat_stratum_completed[[this_outcome]]) - 1
  
  # Calculate AUC...
  fit_final <- glm(use_fit$formula, family = binomial, data = dat_for_variable_selection)
  observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[[this_outcome]]),this_outcome]
  estimated_roc <- roc(observed_vals, fitted.values(fit_final))
  estimated_auc <- as.numeric(estimated_roc$auc)
  
  print(estimated_auc)
  print(use_fit$formula)
}

if((check_convergence_result == TRUE) & (used_intercept_only == TRUE)){
  dummy_list <- as.list(colnames(dat_stratum))
  dummy_list <- lapply(dummy_list, function(x){return("")})
  names(dummy_list) <- colnames(dat_stratum)
  formula_list <- dummy_list
  
  for(i in 1:length(formula_list)){
    fmla <- as.formula(paste(names(formula_list)[i], "~ 1", sep = ""))
    formula_list[[i]] <- fmla
  }
  
  meth_list[[this_outcome]] <- "logreg"
  imp <- mice(data = dat_stratum, 
              m = 1, 
              maxit = use_maxit_value,
              meth =  meth_list,
              formulas = formula_list)
  dat_stratum_completed <- complete(imp, 1)  # Update dat_stratum
  dat_stratum_completed[[this_outcome]] <- as.numeric(dat_stratum_completed[[this_outcome]]) - 1
  
  # Calculate AUC...
  fit_final <- glm(fit$formula, family = binomial, data = dat_for_variable_selection)
  observed_vals <- dat_for_variable_selection[!is.na(dat_for_variable_selection[[this_outcome]]),this_outcome]
  estimated_roc <- roc(observed_vals, fitted.values(fit_final))
  estimated_auc <- as.numeric(estimated_roc$auc)
  
  print(estimated_auc)
  print(fit$formula)
}

# Before we exit...
dat_stratum <- dat_stratum_completed
list_mice_logged_events[[this_outcome]] <- list(imp$loggedEvents)
list_mice_model[[this_outcome]] <- list(imp$formulas[[this_outcome]])

###############################################################################
# Save
###############################################################################
saveRDS(list_mice_logged_events, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("logged_events_stratum", st_num, ".rds", sep = "")))
saveRDS(list_mice_model, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("imputation_model_stratum", st_num, ".rds", sep = "")))
saveRDS(estimated_auc, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("estimated_auc_stratum", st_num, ".rds", sep = "")))
saveRDS(dat_stratum, file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_imputed_stratum", st_num, ".rds", sep = "")))

