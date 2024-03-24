source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
# This dataset contains data from only those sequences which began
# within the 10-day MRT period.
dat_matched_to_decision_points <- readRDS(file = file.path(path_manipulated_data, "dat_matched_to_decision_points.rds"))
# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

################################################################################
# Next, create derived time variables
################################################################################
dat_analysis[["ts_coinflip_local"]] <- convert_to_local_datetime(cleaned_data_frame = dat_analysis, datetime_mountain_name = "ts_coinflip_mountain")
dat_analysis[["hour_coinflip_local"]] <- hour(dat_analysis[["ts_coinflip_local"]]) + minute(dat_analysis[["ts_coinflip_local"]])/60 + second(dat_analysis[["ts_coinflip_local"]])/3600
dat_analysis[["days_between_v1_and_coinflip_local"]] <- int_length(dat_analysis[["v1_date_began_local"]] %--% dat_analysis[["ts_coinflip_local"]])/(60*60*24)
dat_analysis[["is_postquit_local"]] <- if_else(dat_analysis[["days_between_v1_and_coinflip_local"]] > 3.0, 1, 0)
dat_analysis[["days_between_quit_and_coinflip_local"]] <- int_length((dat_analysis[["v1_date_began_local"]] + days(3.0)) %--% dat_analysis[["ts_coinflip_local"]])/(60*60*24)
dat_analysis[["minutes_elapsed_between_block_start_and_rand"]] <- int_length(int_start(dat_analysis[["block_bounds_mountain"]]) %--% dat_analysis[["ts_coinflip_mountain"]])/60

################################################################################
# Save variables
################################################################################
dat_analysis <- dat_analysis %>%
  select(mars_id, decision_point, olson, ts_coinflip_mountain,
         ts_coinflip_local,
         hour_coinflip_local, days_between_v1_and_coinflip_local, 
         is_postquit_local, days_between_quit_and_coinflip_local,
         minutes_elapsed_between_block_start_and_rand)

saveRDS(dat_analysis, file = file.path(path_manipulated_data, "dat_mars_derived_time_vars.rds"))

