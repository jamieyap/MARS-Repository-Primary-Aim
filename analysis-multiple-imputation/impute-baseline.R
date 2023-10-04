source("paths.R")
library(tidyverse)
library(mice)

dat_control_wide <- readRDS(file = file.path(path_multiple_imputation_data, "dat_control_wide.rds"))
dat_baseline_wide <- readRDS(file = file.path(path_multiple_imputation_data, "dat_baseline_wide.rds"))
dat_wide <- full_join(x = dat_control_wide, y = dat_baseline_wide, by = join_by(participant_id == participant_id))

dat_for_imp <- dat_wide %>% select(-participant_id)

###############################################################################
# This step is just vanilla Fully Conditional Specification (FCS)
###############################################################################

imp <- mice(data = dat_for_imp, 
            m = 1,
            maxit = 30)

dat_wide_updated <- complete(imp, 1)
dat_wide_updated[["participant_id"]] <- dat_wide[["participant_id"]]
dat_wide_updated <- dat_wide_updated %>% select(participant_id, everything())

saveRDS(dat_wide_updated, file = file.path(path_multiple_imputation_data, "sequentially-completed-datasets", "dat_completed_baseline.rds"))

