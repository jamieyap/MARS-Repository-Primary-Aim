###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

rm(list = ls())

###############################################################################
# Simulation parameters
###############################################################################
total_replicates <- .__par_total_replicates

################################################################################
# Load packages and datasets
################################################################################
source("paths.R")
library(tidyverse)

dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds"))
all_ids <- unique(dat_primary_aim[["mars_id"]])

################################################################################
# Create replicates
################################################################################
dat_primary_aim <- dat_primary_aim %>% 
  mutate(replicate_id = 0) %>% 
  select(replicate_id, everything())

if(total_replicates > 0){
  list_dat_all <- list()
  list_dat_all <- append(list_dat_all, list(dat_primary_aim))
  
  for(idx in 1:total_replicates){
    dat_replicate <- dat_primary_aim %>% mutate(replicate_id = idx)
    list_dat_all <- append(list_dat_all, list(dat_replicate))
  }
  
  dat_primary_aim <- bind_rows(list_dat_all)
}

################################################################################
# Save
################################################################################
saveRDS(dat_primary_aim, file = file.path(path_multiple_imputation_pipeline_data, "dat_primary_aim_replicated.rds"))

