###############################################################################
# Load packages and datasets
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

mi_dataset_num <- 4 #.__par_mi_number

###############################################################################
# Directory where plots are saved
###############################################################################

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots"))
}

this_location <- file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots", mi_dataset_num)

is_dir_exist <- file.exists(this_location)

if(isFALSE(is_dir_exist)){
  dir.create(this_location)
}

###############################################################################
# Convergence diagnostics
###############################################################################
imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_quick_survey_response_dp1.rds"))
dat_chain_means <- as.data.frame(t(as.matrix(as.data.frame(imp$chainMean))))
dat_chain_vars <- as.data.frame(t(as.matrix(as.data.frame(imp$chainVar))))

# Convert this to a ggplot later on
png(filename = file.path(this_location, "convergence_quick_survey_response_dp1.png"), height = 20, width = 30, units = "in", res = 300)
plot(imp, layout = c(2,1))
dev.off()

###############################################################################
# Convergence diagnostics
###############################################################################
imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_Y_dp1.rds"))
dat_chain_means <- as.data.frame(t(as.matrix(as.data.frame(imp$chainMean))))
dat_chain_vars <- as.data.frame(t(as.matrix(as.data.frame(imp$chainVar))))

# Convert this to a ggplot later on
png(filename = file.path(this_location, "convergence_Y_dp1.png"), height = 20, width = 30, units = "in", res = 300)
plot(imp, layout = c(2,1))
dev.off()

###############################################################################
# Convergence diagnostics
###############################################################################
imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_cigarette_counts_dp1.rds"))
dat_chain_means <- as.data.frame(t(as.matrix(as.data.frame(imp$chainMean))))
dat_chain_vars <- as.data.frame(t(as.matrix(as.data.frame(imp$chainVar))))

# Convert this to a ggplot later on
png(filename = file.path(this_location, "convergence_cigarette_counts_dp1.png"), height = 20, width = 30, units = "in", res = 300)
plot(imp, layout = c(2,1))
dev.off()

###############################################################################
# Convergence diagnostics
###############################################################################
imp <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "imp_obj_src_scored_dp1.rds"))
dat_chain_means <- as.data.frame(t(as.matrix(as.data.frame(imp$chainMean))))
dat_chain_vars <- as.data.frame(t(as.matrix(as.data.frame(imp$chainVar))))

# Convert this to a ggplot later on
png(filename = file.path(this_location, "convergence_src_scored_dp1.png"), height = 20, width = 30, units = "in", res = 300)
plot(imp, layout = c(2,1))
dev.off()



