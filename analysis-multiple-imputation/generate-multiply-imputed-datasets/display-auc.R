rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

###############################################################################
# Read in output from running imputation scripts
###############################################################################
.list_dat_out_stratum1 <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  source("paths.R")
  estimated_auc <- readRDS(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "estimated_auc_stratum1.rds"))
  dat_out <- data.frame(M = mi_dataset_num, auc = estimated_auc)
  .list_dat_out_stratum1 <- append(.list_dat_out_stratum1, list(dat_out))
}

###############################################################################
# Plot results
###############################################################################
dat_out_stratum1 <- bind_rows(.list_dat_out_stratum1)
dat_out_stratum1[["M"]] <- as_factor(dat_out_stratum1[["M"]])
g <- ggplot(data = dat_out_stratum1, mapping = aes(x = M, y = auc))
g + geom_point(size = 0.75) + expand_limits(y = c(0, 1)) + scale_y_continuous(breaks = seq(0,1,0.25)) + geom_hline(yintercept = 0.75, col = "blue", linetype = "dashed") + ggtitle("Stratum 1")
ggsave(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "stratum1.png"), width = 12, height = 12, units = "in")

###############################################################################
# Read in output from running imputation scripts
###############################################################################
.list_dat_out_stratum2 <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  source("paths.R")
  estimated_auc <- readRDS(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "estimated_auc_stratum2.rds"))
  dat_out <- data.frame(M = mi_dataset_num, auc = estimated_auc)
  .list_dat_out_stratum2 <- append(.list_dat_out_stratum2, list(dat_out))
}

###############################################################################
# Plot results
###############################################################################
dat_out_stratum2 <- bind_rows(.list_dat_out_stratum2)
dat_out_stratum2[["M"]] <- as_factor(dat_out_stratum2[["M"]])
g <- ggplot(data = dat_out_stratum2, mapping = aes(x = M, y = auc))
g + geom_point(size = 0.75) + expand_limits(y = c(0, 1)) + scale_y_continuous(breaks = seq(0,1,0.25)) + geom_hline(yintercept = 0.75, col = "blue", linetype = "dashed") + ggtitle("Stratum 2")
ggsave(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "stratum2.png"), width = 12, height = 12, units = "in")

###############################################################################
# Read in output from running imputation scripts
###############################################################################
.list_dat_out_stratum3 <- list()

for(current_dp_value in 2:60){
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    source("paths.R")
    estimated_auc <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("estimated_auc_stratum3_dp", current_dp_value, ".rds", sep = "")))
    dat_out <- data.frame(M = mi_dataset_num, decision_point = current_dp_value, auc = estimated_auc)
    .list_dat_out_stratum3 <- append(.list_dat_out_stratum3, list(dat_out))
  }
}

###############################################################################
# Plot results
###############################################################################
dat_out_stratum3 <- bind_rows(.list_dat_out_stratum3)
dat_out_stratum3[["M"]] <- as_factor(dat_out_stratum3[["M"]])
g <- ggplot(data = dat_out_stratum3, mapping = aes(x = M, y = auc))
g + geom_point(size = 0.75) + expand_limits(y = c(0, 1)) + scale_y_continuous(breaks = seq(0,1,0.25)) + geom_hline(yintercept = 0.75, col = "blue", linetype = "dashed") + facet_wrap(~decision_point) + ggtitle("Stratum 3")
ggsave(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "stratum3.png"), width = 12, height = 12, units = "in")

###############################################################################
# Clean up before exiting
###############################################################################
if(file.exists(file = file.path(path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "Thumbs.db"))){
  file.remove(file = file.path(path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "Thumbs.db"))
}

