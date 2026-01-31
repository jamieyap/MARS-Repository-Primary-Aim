rm(list = ls())

source("paths.R")
source("prepare-analytic-datasets/functions-for-analytical-dataset-creation.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
dat_demogs <- readRDS(file = file.path(path_manipulated_data, "dat_demogs.rds"))
# Among participants who have any data from sequences which began within the
# 10-day MRT period, we identified those participants who did not complete
# at least 3 EMA between the 2nd and 9th day inclusive.
# These participant ID's are saved in the variable below.
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))
mars_ids_pilot <- readRDS(file = file.path(path_manipulated_data, "mars_ids_pilot.rds"))

#-------------------------------------------------------------------------------
# At this point: What we need for clinicaltrials.gov
#-------------------------------------------------------------------------------

################################################################################
# First, drop data from participants which we will not be using in the
# calculation of the summary statistics.
################################################################################
dat_ctgov <- dat_demogs %>% filter(!(mars_id %in% mars_ids_pilot))

################################################################################
# Next, create dichotomous variables for analyses
# Rule of thumb: when dummy coding, 
# assign 0 to the larger group and 1 to the smaller group, except when
# there is a specified reference category to use
################################################################################
dat_ctgov <- dat_ctgov %>% 
  # Gender
  # * female is the larger group and is thus the reference category
  mutate(is_male = if_else(gender_category == "male", 1, 0)) %>%
  mutate(is_female = if_else(gender_category == "female", 1, 0)) %>%
  # Partner status
  # * has no partner is the larger group and is thus the reference category
  mutate(has_partner = if_else((partner_status_category == "married") | (partner_status_category == "living with significant other"), 1, 0)) %>%  
  # Race/ethnicity
  # * Very Important: remember that the reference category specified by collaborators for race/ethnicity is the "not latino and white" category
  # * Turns out, not latino and white and latino categories tie as the largest categories among the four categories
  mutate(is_latino = if_else(race_and_ethnicity == "latino", 1, 0),
         is_not_latino_and_black = if_else(race_and_ethnicity == "not latino and black", 1, 0),
         is_not_latino_and_other = if_else(race_and_ethnicity == "other", 1, 0),
         is_not_latino_and_white = if_else(race_and_ethnicity == "not latino and white", 1, 0))

################################################################################
# Create missing data indicators for counting number of participants 
# having missing values in the demographic variables
################################################################################
dat_ctgov <- dat_ctgov %>%
  mutate(is_missing_age = if_else(is.na(age), 1, 0),
         is_missing_gender = if_else(is.na(gender_category), 1, 0),
         is_missing_race_and_ethnicity = if_else(is.na(race_and_ethnicity), 1, 0),
         is_missing_baseline_tobacco_history = if_else(is.na(baseline_tobacco_history), 1, 0),
         is_missing_partner_status = if_else(is.na(partner_status_category), 1, 0),
         is_missing_income = if_else(is.na(income_val), 1, 0)) %>%
  mutate(is_missing_any_demog_data = if_else(is_missing_age + is_missing_gender + is_missing_race_and_ethnicity + is_missing_baseline_tobacco_history + is_missing_partner_status + is_missing_income >= 1, 1, 0))

################################################################################
# Calculate summary statistics
################################################################################
dat_ctgov_summary_missing_demogs <- dat_ctgov %>%
  summarise(n_participants = n(),
            n_participants_with_missing_demogs = sum(is_missing_any_demog_data),
            n_missing_age = sum(is_missing_age),
            n_missing_gender = sum(is_missing_gender),
            n_missing_race_and_ethnicity = sum(is_missing_race_and_ethnicity),
            n_missing_baseline_tobacco_history = sum(is_missing_baseline_tobacco_history),
            n_missing_partner_status = sum(is_missing_partner_status),
            n_missing_income = sum(is_missing_income))

dat_ctgov_summary_demogs_continuous <- dat_ctgov %>%
  summarise(m_age = mean(age),
            sd_age = sd(age),
            m_baseline_tobacco_history = mean(baseline_tobacco_history, na.rm = TRUE),
            sd_baseline_tobacco_history = sd(baseline_tobacco_history, na.rm = TRUE),
            m_income = mean(income_val, na.rm = TRUE),
            sd_income = sd(income_val, na.rm = TRUE),
            median_income = median(income_val, na.rm = TRUE))

dat_ctgov_summary_demogs_binary <- dat_ctgov %>%
  summarise(n_male = sum(is_male),
            pct_male = mean(is_male) * 100,
            n_latino = sum(is_latino),
            pct_latino = mean(is_latino) * 100,
            n_not_latino = sum(is_latino == 0),
            pct_not_latino = mean(is_latino == 0),
            n_not_latino_and_black = sum(is_not_latino_and_black),
            pct_not_latino_and_black = mean(is_not_latino_and_black) * 100,
            n_not_latino_and_other = sum(is_not_latino_and_other),
            pct_not_latino_and_other = mean(is_not_latino_and_other) * 100,
            n_not_latino_and_white = sum(is_not_latino_and_white),
            pct_not_latino_and_white = mean(is_not_latino_and_white) * 100,
            n_has_partner = sum(has_partner, na.rm = TRUE),
            pct_has_partner = mean(has_partner, na.rm = TRUE) * 100)

dat_ctgov_summary_income_tabulation <- dat_ctgov %>% group_by(income_val) %>% summarise(num_participants = n())

################################################################################
# Save output in csv format
################################################################################
is_dir_exist <- file.exists(file.path("analysis-complete-case", "formatted-output", "ctgov"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("analysis-complete-case", "formatted-output", "ctgov"))
}

write.csv(dat_ctgov_summary_missing_demogs, file.path("analysis-complete-case", "formatted-output", "ctgov", "ctgov_dat_summary_missing_demogs.csv"), row.names = FALSE)
write.csv(dat_ctgov_summary_demogs_continuous, file.path("analysis-complete-case", "formatted-output", "ctgov", "ctgov_dat_summary_demogs_continuous.csv"), row.names = FALSE)
write.csv(dat_ctgov_summary_demogs_binary, file.path("analysis-complete-case", "formatted-output", "ctgov", "ctgov_dat_summary_demogs_binary.csv"), row.names = FALSE)
write.csv(dat_ctgov_summary_income_tabulation, file.path("analysis-complete-case", "formatted-output", "ctgov", "ctgov_dat_summary_income_tabulation.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# From here onward: What we need for consort
#-------------------------------------------------------------------------------

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_demogs %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

################################################################################
# Next, create dichotomous variables for analyses
# Rule of thumb: when dummy coding, 
# assign 0 to the larger group and 1 to the smaller group, except when
# there is a specified reference category to use
################################################################################
dat_analysis <- dat_analysis %>% 
  # Gender
  # * female is the larger group and is thus the reference category
  mutate(is_male = if_else(gender_category == "male", 1, 0)) %>%
  mutate(is_female = if_else(gender_category == "female", 1, 0)) %>%
  # Partner status
  # * has no partner is the larger group and is thus the reference category
  mutate(has_partner = if_else((partner_status_category == "married") | (partner_status_category == "living with significant other"), 1, 0)) %>%  
  # Race/ethnicity
  # * Very Important: remember that the reference category specified by collaborators for race/ethnicity is the "not latino and white" category
  # * Turns out, not latino and white and latino categories tie as the largest categories among the four categories
  mutate(is_latino = if_else(race_and_ethnicity == "latino", 1, 0),
         is_not_latino_and_black = if_else(race_and_ethnicity == "not latino and black", 1, 0),
         is_not_latino_and_other = if_else(race_and_ethnicity == "other", 1, 0),
         is_not_latino_and_white = if_else(race_and_ethnicity == "not latino and white", 1, 0))

################################################################################
# Create missing data indicators for counting number of participants 
# having missing values in the demographic variables
################################################################################
dat_analysis <- dat_analysis %>%
  mutate(is_missing_age = if_else(is.na(age), 1, 0),
         is_missing_gender = if_else(is.na(gender_category), 1, 0),
         is_missing_race_and_ethnicity = if_else(is.na(race_and_ethnicity), 1, 0),
         is_missing_baseline_tobacco_history = if_else(is.na(baseline_tobacco_history), 1, 0),
         is_missing_partner_status = if_else(is.na(partner_status_category), 1, 0),
         is_missing_income = if_else(is.na(income_val), 1, 0)) %>%
  mutate(is_missing_any_demog_data = if_else(is_missing_age + is_missing_gender + is_missing_race_and_ethnicity + is_missing_baseline_tobacco_history + is_missing_partner_status + is_missing_income >= 1, 1, 0))

################################################################################
# Calculate summary statistics
################################################################################
dat_summary_missing_demogs <- dat_analysis %>%
  summarise(n_participants = n(),
            n_participants_with_missing_demogs = sum(is_missing_any_demog_data),
            n_missing_age = sum(is_missing_age),
            n_missing_gender = sum(is_missing_gender),
            n_missing_race_and_ethnicity = sum(is_missing_race_and_ethnicity),
            n_missing_baseline_tobacco_history = sum(is_missing_baseline_tobacco_history),
            n_missing_partner_status = sum(is_missing_partner_status),
            n_missing_income = sum(is_missing_income))

dat_summary_demogs_continuous <- dat_analysis %>%
  summarise(m_age = mean(age),
            sd_age = sd(age),
            m_baseline_tobacco_history = mean(baseline_tobacco_history, na.rm = TRUE),
            sd_baseline_tobacco_history = sd(baseline_tobacco_history, na.rm = TRUE),
            m_income = mean(income_val, na.rm = TRUE),
            sd_income = sd(income_val, na.rm = TRUE))

dat_summary_demogs_binary <- dat_analysis %>%
  summarise(n_male = sum(is_male),
            pct_male = mean(is_male) * 100,
            n_latino = sum(is_latino),
            pct_latino = mean(is_latino) * 100,
            n_not_latino = sum(is_latino == 0),
            pct_not_latino = mean(is_latino == 0),
            n_not_latino_and_black = sum(is_not_latino_and_black),
            pct_not_latino_and_black = mean(is_not_latino_and_black) * 100,
            n_not_latino_and_other = sum(is_not_latino_and_other),
            pct_not_latino_and_other = mean(is_not_latino_and_other) * 100,
            n_not_latino_and_white = sum(is_not_latino_and_white),
            pct_not_latino_and_white = mean(is_not_latino_and_white) * 100,
            n_has_partner = sum(has_partner, na.rm = TRUE),
            pct_has_partner = mean(has_partner, na.rm = TRUE) * 100)

dat_summary_income_tabulation <- dat_analysis %>% group_by(income_val) %>% summarise(num_participants = n())

################################################################################
# Select only the columns needed
################################################################################
dat_analysis <- dat_analysis %>% 
  select(mars_id, redcap_id, rsr_id, is_missing_any_demog_data,
         age,
         is_male, is_female, 
         is_latino, is_not_latino_and_black, is_not_latino_and_other, # these three indicators collectively define race/ethnicity
         baseline_tobacco_history, 
         has_partner, 
         income_val)

################################################################################
# Save output in RDS format
################################################################################
saveRDS(dat_summary_missing_demogs, file = file.path(path_manipulated_data, "dat_summary_missing_demogs.rds"))
saveRDS(dat_summary_demogs_continuous, file = file.path(path_manipulated_data, "dat_summary_demogs_continuous.rds"))
saveRDS(dat_summary_demogs_binary, file = file.path(path_manipulated_data, "dat_summary_demogs_binary.rds"))

saveRDS(dat_analysis, file = file.path(path_manipulated_data, "dat_mars_coded_demogs.rds"))

################################################################################
# Save output in csv format
################################################################################
is_dir_exist <- file.exists(file.path("analysis-complete-case", "formatted-output"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("analysis-complete-case", "formatted-output"))
}

is_dir_exist <- file.exists(file.path("analysis-complete-case", "formatted-output", "summary-statistics"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path("analysis-complete-case", "formatted-output", "summary-statistics"))
}

write.csv(dat_summary_missing_demogs, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_missing_demogs.csv"), row.names = FALSE)
write.csv(dat_summary_demogs_continuous, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_demogs_continuous.csv"), row.names = FALSE)
write.csv(dat_summary_demogs_binary, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_demogs_binary.csv"), row.names = FALSE)
write.csv(dat_summary_income_tabulation, file.path("analysis-complete-case", "formatted-output", "summary-statistics", "dat_summary_income_tabulation.csv"), row.names = FALSE)

