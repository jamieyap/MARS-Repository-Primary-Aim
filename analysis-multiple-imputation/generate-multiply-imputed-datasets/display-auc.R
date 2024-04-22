rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Read in output from running imputation scripts
###############################################################################
.list_dat_out_stratum_01 <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  source("paths.R")
  estimated_auc <- readRDS(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "estimated_auc_stratum_01.rds"))
  dat_out <- data.frame(M = mi_dataset_num, auc = estimated_auc)
  .list_dat_out_stratum_01 <- append(.list_dat_out_stratum_01, list(dat_out))
}

.list_dat_out_stratum_02 <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  source("paths.R")
  estimated_auc <- readRDS(file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "estimated_auc_stratum_02.rds"))
  dat_out <- data.frame(M = mi_dataset_num, auc = estimated_auc)
  .list_dat_out_stratum_02 <- append(.list_dat_out_stratum_02, list(dat_out))
}

###############################################################################
# Plot results
###############################################################################
library(tidyverse)

dat_out_stratum_01 <- bind_rows(.list_dat_out_stratum_01)
dat_out_stratum_02 <- bind_rows(.list_dat_out_stratum_02)
dat_out_stratum_01[["stratum"]] <- 1
dat_out_stratum_02[["stratum"]] <- 2
dat_out_stratum_01_and_02 <- rbind(dat_out_stratum_01, dat_out_stratum_02)
dat_out_stratum_01_and_02[["stratum"]] <- as_factor(dat_out_stratum_01_and_02[["stratum"]])
g <- ggplot(data = dat_out_stratum_01_and_02, mapping = aes(x = M, y = auc, color = stratum))
g + geom_line(aes(linetype = stratum), linewidth = 3) + expand_limits(y = c(0, 1))
ggsave(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "stratum_01_and_02.png"), width = 8, height = 5, units = "in")

###############################################################################
# Read in output from running imputation scripts
###############################################################################
.list_dat_out_stratum_03 <- list()

for(current_dp_value in 2:60){
  for(mi_dataset_num in 1:.__total_imputed_datasets){
    source("paths.R")
    estimated_auc <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("estimated_auc_dp", current_dp_value, ".rds", sep = "")))
    dat_out <- data.frame(M = mi_dataset_num, decision_point = current_dp_value, auc = estimated_auc)
    .list_dat_out_stratum_03 <- append(.list_dat_out_stratum_03, list(dat_out))
  }
}

###############################################################################
# Plot results
###############################################################################
library(tidyverse)

dat_out_stratum_03 <- bind_rows(.list_dat_out_stratum_03)
dat_out_stratum_03[["M"]] <- as_factor(dat_out_stratum_03[["M"]])
g <- ggplot(data = dat_out_stratum_03, mapping = aes(x = decision_point, y = auc, color = M))
g + geom_point(aes(color = M, shape = M, size = 4)) + geom_line(aes(color = M), linewidth = 1) + expand_limits(y = c(0, 1))
ggsave(file.path(.__path_code, "analysis-multiple-imputation", "generate-multiply-imputed-datasets", "stratum_03.png"), width = 8, height = 5, units = "in")

