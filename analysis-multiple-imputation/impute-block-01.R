source("paths.R")

###############################################################################
# In this script, we begin to make modifications to vanilla
# Fully Conditional Specification (FCS) to accomodate restrictions imposed
# by the design of the MRT
###############################################################################

library(tidyverse)
library(mice)

dat_completed_baseline <- readRDS(file = file.path(path_multiple_imputation_data, "sequentially-completed-datasets", "dat_completed_baseline.rds"))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_data, "dat_timevarying_wide.rds"))

###############################################################################
# Step 1: Impute the primary proximal outcome
###############################################################################

dat_current_dp_outcome <- dat_timevarying_wide %>% select(participant_id, Y_dp1, eligibility_dp1, coinflip_dp1, any_response_2qs_dp1)
dat_current_dp_outcome <- left_join(x = dat_completed_baseline, y = dat_current_dp_outcome, by = join_by(participant_id == participant_id))

# Specifying the correct restriction is key to imputing MRT data.
# At the first decision point ever of the trial, we do not really have any
# EMAs assessed prior. Thus, the relevant restriction to respect is simply
# to impute missing values of Y_1 at blocks which had a micro-randomization.
restriction_meet_string <- "eligibility_dp1 == 1"
restriction_violate_string <- "eligibility_dp1 == 0"

# We employ a nifty coding trick that will allow us the option to put all these
# computations in a loop or function at a later time.
# This coding trick allows us to filter based on a string by calling the
# parse_expr function from the rlang package.
#
# More about this coding trick here:
# https://www.r-bloggers.com/2020/09/using-dplyrfilter-when-the-condition-is-a-string/
rows_meet_restriction <- dat_current_dp_outcome %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_current_dp_outcome %>% filter(!!rlang::parse_expr(restriction_violate_string))

my_list <- list()
my_list[["Y_dp1"]] <- as.formula(paste("Y_dp1 ~ coinflip_dp1 + any_response_2qs_dp1 + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + gratitude"))  
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 30,
            formulas =  my_list)

rows_meet_restriction_updated <- complete(imp, 1) 
dat_current_dp_outcome_updated <- rbind(rows_meet_restriction_updated, rows_violate_restriction)

###############################################################################
# Step 2: Impute the other time-varying covariates measured via EMA (the same
# EMA at which the primary proximal outcome was also assessed)
###############################################################################

dat_current_dp_covariates <- dat_timevarying_wide %>%
  select(participant_id,
         src_scored_dp1,
         cigarette_counts_dp1)

dat_current_dp_covariates <- left_join(x = dat_current_dp_covariates, y = dat_current_dp_outcome_updated, by = join_by(participant_id == participant_id))

restriction_meet_string <- "eligibility_dp1 == 1"
restriction_violate_string <- "eligibility_dp1 == 0"

rows_meet_restriction <- dat_current_dp_covariates %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_current_dp_covariates %>% filter(!!rlang::parse_expr(restriction_violate_string))

my_list <- list()
my_list[["src_scored_dp1"]] <- as.formula(paste("src_scored_dp1 ~ Y_dp1 + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + gratitude"))
my_list[["cigarette_counts_dp1"]] <- as.formula(paste("cigarette_counts_dp1 ~ Y_dp1 + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + gratitude"))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 30,
            formulas =  my_list)

rows_meet_restriction_updated <- complete(imp, 1) 
dat_current_dp_covariates_updated <- rbind(rows_meet_restriction_updated, rows_violate_restriction)

###############################################################################
# Step 3: Now you may move on to the next block
###############################################################################

saveRDS(dat_current_dp_covariates_updated, file = file.path(path_multiple_imputation_data, "sequentially-completed-datasets", "dat_completed_dp1.rds"))

