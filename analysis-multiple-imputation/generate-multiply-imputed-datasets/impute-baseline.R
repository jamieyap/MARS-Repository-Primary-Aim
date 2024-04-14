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
# Set up imputation method, including variables which will be imputed passively
###############################################################################
meth_list[["is_male"]] <- ""
meth_list[["has_partner"]] <- "logreg"
meth_list[["income_val"]] <- "pmm"
meth_list[["income_val_squared"]] <- "~ I(income_val * income_val)"
meth_list[["FinancialStrain"]] <- "pmm"
meth_list[["FinancialStrain_squared"]] <- "~ I(FinancialStrain * FinancialStrain)"
meth_list[["nd_mean"]] <- "pmm"
meth_list[["food_security_mean"]] <- "pmm"
meth_list[["SSSladders"]] <- "pmm"
meth_list[["pp1_1"]] <- "pmm"
meth_list[["sni_count"]] <- "pmm"
meth_list[["sni_count_squared"]] <- "~ I(sni_count * sni_count)"
meth_list[["sni_active"]] <- "pmm"
meth_list[["sni_active_squared"]] <- "~ I(sni_active * sni_active)"
meth_list[["sni_people"]] <- "pmm"
meth_list[["isel_total"]] <- "pmm"
meth_list[["isel_belonging"]] <- "pmm"
meth_list[["isel_appraisal"]] <- "pmm"

###############################################################################
# Set up formulas
###############################################################################
pred_mat[,"replicate_id"] <- 0
pred_mat[,"participant_id"] <- 0

pred_mat["income_val","income_val_squared"] <- 0
pred_mat["income_val_squared",] <- 0

pred_mat["FinancialStrain","FinancialStrain_squared"] <- 0
pred_mat["FinancialStrain_squared",] <- 0

pred_mat["sni_count","sni_count_squared"] <- 0
pred_mat["sni_count_squared",] <- 0

pred_mat["sni_active","sni_active_squared"] <- 0
pred_mat["sni_active_squared",] <- 0

###############################################################################
# Create the imputations
###############################################################################
imp <- mice(data = dat_wide, 
            m = .__total_imputed_datasets,
            maxit = .__par_maxit_value_baseline,
            meth = meth_list,
            predictorMatrix = pred_mat)

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
  dat_wide_completed <- complete(imp, mi_dataset_num)
  dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")
  dat_wide_completed <- dat_wide_completed %>% mutate(mi_dataset_number = mi_dataset_num)
  dat_wide_completed[["has_partner"]] <- as.numeric(dat_wide_completed[["has_partner"]]) - 1
  saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))
}

