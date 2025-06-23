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
# Calculate aggregate summary statistics about outcomes
###############################################################################
# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

dat_summary_outcomes <- dat_primary_aim %>%
  summarise(prop_Y = mean(Y, na.rm = TRUE),
            prop_cig = mean(substance_is_cigarettes, na.rm = TRUE),
            prop_nic = mean(substance_is_any_nicotine, na.rm = TRUE))

print(dat_summary_outcomes)

# > print(dat_summary_outcomes)
# prop_Y prop_cig  prop_nic
# 1 0.6191042 0.334409 0.3440904