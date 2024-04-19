rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

dat_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_baseline_wide.rds"))
dat_wide[["has_partner"]] <- as_factor(dat_wide[["has_partner"]])

dat_wide <- dat_wide %>% filter(replicate_id == 0)

###############################################################################
# Create directories to store sequentially completed datasets
###############################################################################

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))
}

###############################################################################
# Initialize lists which will store imputation method and formula
###############################################################################
imp0 <- mice(data = dat_wide, 
             m = 1, 
             maxit = 0)
meth_list <- imp0$method
formula_list <- imp0$formulas
pred_mat <- imp0$predictorMatrix

###############################################################################
# Set up imputation method
# Note that since we are simply using defaults, this step is not really
# necessary, but it is helpful to see what a template would look like in
# case we needed to employ passive imputation at some point
# e.g., this could look like 
# meth_list[["FinancialStrainSquared"]] <- "~I(FinancialStrain * FinancialStrain)"
###############################################################################

# demographic variables
meth_list[["age"]] <- "" 
meth_list[["is_male"]] <- ""                                  # -- this variable does not have missing values
meth_list[["has_partner"]] <- "logreg"

# baseline tobacco dependence
meth_list[["baseline_tobacco_history"]] <- ""                 # -- this variable does not have missing values
meth_list[["Nicotine_dep"]] <- "pmm"

# baseline socio-economic status
meth_list[["income_val"]] <- "pmm"
meth_list[["FinancialStrain"]] <- "pmm"
meth_list[["nd_mean"]] <- "pmm"
meth_list[["food_security_mean"]] <- "pmm"
meth_list[["SSSladders"]] <- "pmm"
meth_list[["pp1_1"]] <- "pmm"

# baseline agency
meth_list[["srq_mean"]] <- "pmm"
meth_list[["se_social"]] <- "pmm"
meth_list[["se_habit"]] <- "pmm"
meth_list[["se_negaff"]] <- "pmm"

# baseline social support
meth_list[["sni_count"]] <- "pmm"
meth_list[["sni_active"]] <- "pmm"
meth_list[["sni_people"]] <- "pmm"
meth_list[["isel_belonging"]] <- "pmm"
meth_list[["isel_appraisal"]] <- "pmm"
meth_list[["isel_tangible"]] <- "pmm"

###############################################################################
# Set up formulas
###############################################################################

# Since these variables are ID's, they should never be included in any imputation model
pred_mat[,"replicate_id"] <- 0
pred_mat[,"participant_id"] <- 0
# This variable will not be included in any of the imputation models for baseline variables
pred_mat[,"is_complete_v1_quest"] <- 0

###############################################################################
# Create the imputations
###############################################################################
imp <- mice(data = dat_wide, 
            m = .__total_imputed_datasets,
            maxit = .__par_maxit_value_baseline,
            meth = meth_list,
            predictorMatrix = pred_mat)

###############################################################################
# Sanity check convergence
###############################################################################
if(FALSE){
  print(imp$loggedEvents)  # Are there any warnings?
  plot(imp)
  bwplot(imp)  # Graphically compare the distribution of the observed values against the distribution of the imputed values
  stripplot(imp)  # Graphically compare the distribution of the observed values against the distribution of the imputed values
}

###############################################################################
# Save completed datasets
###############################################################################
for(mi_dataset_num in 1:.__total_imputed_datasets){
  is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))
  if(isFALSE(is_dir_exist)){
    dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))
  }
}

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", "imp_obj_baseline.rds"))

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_wide_completed0 <- complete(imp, mi_dataset_num)
  
  list_all <- list()
  for(idx_replicate in 0:.__par_total_replicates){
    dat_wide_completed1 <- dat_wide_completed0 %>% mutate(replicate_id = idx_replicate)
    list_all <- append(list_all, list(dat_wide_completed1))
  }
  dat_wide_completed <- bind_rows(list_all)
  
  dat_wide_completed <- arrange(dat_wide_completed, replicate_id, participant_id)
  dat_wide_completed <- dat_wide_completed %>% mutate(mi_dataset_number = mi_dataset_num)
  dat_wide_completed[["has_partner"]] <- as.numeric(dat_wide_completed[["has_partner"]]) - 1  # convert from factor to numeric type
  saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
}

