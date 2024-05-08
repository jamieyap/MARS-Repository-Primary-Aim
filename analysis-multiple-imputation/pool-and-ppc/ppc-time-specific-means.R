rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

###############################################################################
# Posterior predictive check for time-specific means
###############################################################################
list_all <- list()

for(mi_dataset_num in 1:.__total_imputed_datasets){
  dat_long_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, paste("dat_long_completed", ".rds", sep = "")))
  dat_current <- dat_long_completed %>%
    filter(eligibility == 1) %>%
    group_by(replicate_id, decision_point) %>%
    summarise(mu_cigarette_availability = mean(cigarette_availability, na.rm = TRUE),
              mu_engage = mean(Y, na.rm = TRUE),
              mu_cigarette_counts = mean(cigarette_counts, na.rm = TRUE),
              mu_src_scored = mean(src_scored, na.rm = TRUE))
  dat0 <- dat_current %>% filter(replicate_id == 0)
  dat1 <- dat_current %>% filter(replicate_id == 1)
  
  dp_array <- dat0$decision_point
  dat_compared <- (dat1 >= dat0)
  dat_compared <- as.data.frame(dat_compared)
  dat_compared <- dat_compared %>% select(-replicate_id) %>% mutate(mi_num = mi_dataset_num, decision_point = dp_array)
  list_all <- append(list_all, list(dat_compared))
}

dat_all <- bind_rows(list_all)

dat_pbcom <- dat_all %>%
  group_by(decision_point) %>%
  summarise(prob_mu_cigarette_availability = mean(mu_cigarette_availability),
            prob_mu_engage = mean(mu_engage),
            prob_mu_cigarette_counts = mean(mu_cigarette_counts),
            prob_mu_src_scored = mean(mu_src_scored))

write.csv(dat_pbcom, file = file.path("analysis-multiple-imputation", "formatted-output", "pbcom_by_dp.csv"), row.names = TRUE)
