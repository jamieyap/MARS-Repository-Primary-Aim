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

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
library(mice)

dat_control_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_control_wide.rds"))
dat_baseline_wide <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "dat_baseline_wide.rds"))
dat_wide <- full_join(x = dat_control_wide, y = dat_baseline_wide, by = join_by(participant_id == participant_id))

dat_wide <- arrange(dat_wide, by = "participant_id")
dat_wide <- dat_wide %>% mutate(mi_dataset_number = mi_dataset_num) %>% select(mi_dataset_number, everything())
dat_wide_init <- dat_wide

###############################################################################
# Create directories to store sequentially completed datasets
###############################################################################

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets"))
}

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num))
}

###############################################################################
# Specify a regression model for each variable individually
###############################################################################
my_list <- list()
my_list[["baseline_tobacco_history"]] <- as.formula(paste("baseline_tobacco_history ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other"))
my_list[["has_partner"]] <- as.formula(paste("has_partner ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + income_val + is_income_observed"))
my_list[["income_val"]] <- as.formula(paste("income_val ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + has_partner + srq_mean"))
my_list[["srq_mean"]] <- as.formula(paste("srq_mean ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + income_val + ffmq_nonjudge + is_income_observed"))
my_list[["mdes_pos_mean"]] <- as.formula(paste("mdes_pos_mean ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + mdes_neg_mean + gratitude + ffmq_nonjudge"))
my_list[["mdes_neg_mean"]] <- as.formula(paste("mdes_neg_mean ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + mdes_pos_mean + gratitude + ffmq_nonjudge"))
my_list[["maas_total"]] <- as.formula(paste("maas_total ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + ffmq_nonjudge"))
my_list[["ffmq_nonjudge"]] <- as.formula(paste("ffmq_nonjudge ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + mdes_pos_mean + mdes_neg_mean + maas_total"))
my_list[["gratitude"]] <- as.formula(paste("gratitude ~ age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + mdes_pos_mean + mdes_neg_mean"))

imp <- mice(data = dat_wide, 
            m = 1,
            maxit = use_maxit_value,
            formulas = my_list)

dat_wide_completed <- complete(imp, 1)
dat_wide_completed <- arrange(dat_wide_completed, by = "participant_id")

saveRDS(imp, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_baseline.rds"))

###############################################################################
# Save
###############################################################################
saveRDS(dat_wide_init, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_init_baseline.rds"))

saveRDS(dat_wide_completed, file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))


