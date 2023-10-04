source("paths.R")

###############################################################################
# In this script, we begin to make modifications to vanilla
# Fully Conditional Specification (FCS) to accomodate restrictions imposed
# by the design of the MRT
###############################################################################

library(tidyverse)
library(mice)

now <- 7

dat_completed_history <- readRDS(file = file.path(path_multiple_imputation_data, "sequentially-completed-datasets", paste("dat_completed_dp", now-1, ".rds", sep = "")))
dat_timevarying_wide <- readRDS(file = file.path(path_multiple_imputation_data, "dat_timevarying_wide.rds"))

###############################################################################
# Step 1: Consider first set of restrictions
# 
# Step 1a: Create predictors for the primary proximal outcome imputation model
# Note that predictors would only be created for those rows which meet 
# restrictions
###############################################################################

colnames_of_completed <- dat_completed_history %>% select(-participant_id) %>% colnames(.)
dat_timevarying_wide <- dat_timevarying_wide %>% select(-any_of(colnames_of_completed))
dat_timevarying_wide <- left_join(x = dat_completed_history, y = dat_timevarying_wide, by = join_by(participant_id == participant_id))

dat_current_dp_outcome <- dat_timevarying_wide %>% 
  select(participant_id, Y_dp7, eligibility_dp7, coinflip_dp7, any_response_2qs_dp7, elig24hrs_dp7, matched_24hrs_dp7, any_recent_eligible_dp_dp7, engagement_most_recent_eligible_dp7, matched_recent_dp7,
         age, is_female, is_latino, is_not_latino_and_black, is_not_latino_and_other, baseline_tobacco_history, has_partner, income_val, srq_mean, mdes_neg_mean, gratitude)

# For rows that meet restrictions, the value of these variables will be udpated.
# For rows that do not meet restrictions, the value of these variables will not be updated (and thus are coded as -1 in the completed dataset).
dat_current_dp_outcome[["Y_sum_past24hrs_dp7"]] <- -1
dat_current_dp_outcome[["src_scored_mean_past24hrs_dp7"]] <- -1
dat_current_dp_outcome[["cigarette_counts_sum_past24hrs_dp7"]] <- -1

restriction_meet_string <- "eligibility_dp7 == 1 & elig24hrs_dp7 == 1"
restriction_violate_string <- "!(eligibility_dp7 == 1 & elig24hrs_dp7 == 1)"
rows_meet_restriction <- dat_current_dp_outcome %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_current_dp_outcome %>% filter(!!rlang::parse_expr(restriction_violate_string))
ids_meet_restriction <- rows_meet_restriction[["participant_id"]]

for(i in 1:nrow(rows_meet_restriction)){
  current_matched <- rows_meet_restriction[["matched_24hrs_dp7"]][i]
  indices24hrs <- current_matched:(now-1)
  colnames24hrs_eligibility <- paste("eligibility_dp", indices24hrs, sep = "")
  colnames24hrs_Y <- paste("Y_dp", indices24hrs, sep = "")
  colnames24hrs_src_scored <- paste("src_scored_dp", indices24hrs, sep = "")
  colnames24hrs_cigarette_counts <- paste("cigarette_counts_dp", indices24hrs, sep = "")
  
  dat_completed_history_meet_restriction <- dat_timevarying_wide %>% filter(participant_id == ids_meet_restriction[i])
  dat_eligibility <- dat_completed_history_meet_restriction %>% select(all_of(colnames24hrs_eligibility))
  dat_Y <- dat_completed_history_meet_restriction %>% select(all_of(colnames24hrs_Y))
  dat_src_scored <- dat_completed_history_meet_restriction %>% select(all_of(colnames24hrs_src_scored))
  dat_cigarette_counts <- dat_completed_history_meet_restriction %>% select(all_of(colnames24hrs_cigarette_counts))
  
  rows_meet_restriction[["Y_sum_past24hrs_dp7"]][i] <- rowSums(dat_eligibility * dat_Y)
  rows_meet_restriction[["src_scored_mean_past24hrs_dp7"]][i] <- rowSums(dat_eligibility * dat_src_scored)
  rows_meet_restriction[["cigarette_counts_sum_past24hrs_dp7"]][i] <- rowSums(dat_eligibility * dat_cigarette_counts)
}

for(i in 1:nrow(rows_meet_restriction)){
  # Note that this restriction is passed by rows which have a micro-randomization in the past 24 hours (excluding the current decision point)
  passed_condition <- rows_meet_restriction[["any_recent_eligible_dp_dp7"]][i]
  
  if(passed_condition == 1){
    current_matched <- rows_meet_restriction[["matched_recent_dp7"]][i]
    colnamesrecent_Y <- paste("Y_dp", current_matched, sep = "")
    
    dat_completed_history_meet_restriction <- dat_timevarying_wide %>% filter(participant_id == ids_meet_restriction[i])
    dat_Y <- dat_completed_history_meet_restriction %>% select(all_of(colnamesrecent_Y))
    rows_meet_restriction[["engagement_most_recent_eligible_dp7"]][i] <- dat_Y[[1]]
  }
}

###############################################################################
# Step 1: Consider first set of restrictions
# 
# Step 1b: Specify imputation model for primary proximal outcome
###############################################################################

my_list <- list()

# Note that at dp = 3, Y_sum_past24hrs_dp7 and engagement_most_recent_eligible_dp7 DO NOT have identical values. Hence, we only include both.
# Also note that there is no need to include any_recent_eligible_dp_dp7 since all rows who meet our past24 hour restriction will have a value of 1 for this variable.
my_list[["Y_dp7"]] <- as.formula(paste("Y_dp7 ~ coinflip_dp7 + any_response_2qs_dp7 + Y_sum_past24hrs_dp7 + engagement_most_recent_eligible_dp7 + src_scored_mean_past24hrs_dp7 + cigarette_counts_sum_past24hrs_dp7 + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val"))  
imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 30,
            formulas =  my_list)

rows_meet_restriction_updated <- complete(imp, 1) 
dat_current_dp_outcome_updated <- rbind(rows_meet_restriction_updated, rows_violate_restriction)

###############################################################################
# Step 2: Consider another set of restrictions
#
# Specify (very simple) imputation model for primary proximal outcome
###############################################################################

restriction_meet_string <- "eligibility_dp7 == 1 & elig24hrs_dp7 == 0"
restriction_violate_string <- "!(eligibility_dp7 == 1 & elig24hrs_dp7 == 0)"
rows_meet_restriction <- dat_current_dp_outcome_updated %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_current_dp_outcome_updated %>% filter(!!rlang::parse_expr(restriction_violate_string))
ids_meet_restriction <- rows_meet_restriction[["participant_id"]]

prob_when_prompt <- 0.6
prob_when_none <- 0.4

indices_to_impute <- is.na(rows_meet_restriction[["Y_dp7"]])
rows_meet_restriction[["Y_dp7"]][indices_to_impute] <- rows_meet_restriction[["coinflip_dp7"]][indices_to_impute] * rbinom(n = sum(indices_to_impute), size = 1, prob = prob_when_prompt) + (1 - rows_meet_restriction[["coinflip_dp7"]][indices_to_impute]) * rbinom(n = sum(indices_to_impute), size = 1, prob = prob_when_none)

dat_current_dp_outcome_updated <- rbind(rows_meet_restriction, rows_violate_restriction)

###############################################################################
# Step 3: Impute the other time-varying covariates measured via EMA (the same
# EMA at which the primary proximal outcome was also assessed)
###############################################################################

dat_current_dp_covariates <- dat_timevarying_wide %>%
  select(participant_id,
         src_scored_dp7,
         cigarette_counts_dp7)

dat_current_dp_covariates <- left_join(x = dat_current_dp_covariates, y = dat_current_dp_outcome_updated, by = join_by(participant_id == participant_id))

restriction_meet_string <- "eligibility_dp7 == 1"
restriction_violate_string <- "eligibility_dp7 == 0"

rows_meet_restriction <- dat_current_dp_covariates %>% filter(!!rlang::parse_expr(restriction_meet_string))
rows_violate_restriction <- dat_current_dp_covariates %>% filter(!!rlang::parse_expr(restriction_violate_string))

my_list <- list()
my_list[["src_scored_dp7"]] <- as.formula(paste("src_scored_dp7 ~ Y_dp7 + cigarette_counts_dp7 + srq_mean + mdes_neg_mean + gratitude + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val"))
my_list[["cigarette_counts_dp7"]] <- as.formula(paste("cigarette_counts_dp7 ~ Y_dp7 + src_scored_dp7 + srq_mean + mdes_neg_mean + gratitude + age + is_female + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val"))

imp <- mice(data = rows_meet_restriction, 
            m = 1, 
            maxit = 30,
            formulas =  my_list)

rows_meet_restriction_updated <- complete(imp, 1) 
dat_current_dp_covariates_updated <- rbind(rows_meet_restriction_updated, rows_violate_restriction)

###############################################################################
# Step 4: Now you may move on to the next block
###############################################################################

cols_completed <- colnames(dat_current_dp_covariates_updated)
cols_completed <- cols_completed[cols_completed != "participant_id"]
dat_future <- dat_timevarying_wide %>% select(!any_of(cols_completed))
dat_current_dp_updated <- left_join(x = dat_current_dp_covariates_updated, y = dat_future, by = join_by(participant_id == participant_id))

saveRDS(dat_current_dp_updated, file = file.path(path_multiple_imputation_data, "sequentially-completed-datasets", paste("dat_completed_dp", now, ".rds", sep = "")))

