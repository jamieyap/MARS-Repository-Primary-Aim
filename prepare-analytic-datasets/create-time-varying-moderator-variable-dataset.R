rm(list = ls())

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
# Contains masterlist of EMA questions
dat_master_ema_questions <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_questions.rds"))
# Contains masterlist of response options, response types, response options related to each EMA question
dat_master_ema_response_options <- readRDS(file = file.path(path_manipulated_data, "dat_master_ema_response_options.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
dat_analysis <- dat_matched_to_decision_points %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))
all_ids <- unique(dat_analysis[["mars_id"]])
n_ids <- length(all_ids)

################################################################################
# Keep track of column names you will want to keep
################################################################################
keep_these_columns_for_analysis <- list()

################################################################################
# Collapse EMA completion status into a few levels
################################################################################
dat_analysis[["status_survey_ema_collapsed"]] <- collapse_survey_ema_status(cleaned_data_frame = dat_analysis)
dat_analysis[["total_ema_items_with_response"]] <- count_total_items_with_response_ema(cleaned_data_frame = dat_analysis)

keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(c("status_survey_ema_collapsed", "total_ema_items_with_response")))

################################################################################
# Read in dataset which already identifies micro-randomizations in the past
# 24 hours
################################################################################
scanned_decision_points_within_range <- readRDS(file = file.path(path_manipulated_data, "scanned_decision_points_within_range.rds"))
new_cols <- colnames(scanned_decision_points_within_range)
dat_analysis <- left_join(x = dat_analysis, 
                          y = scanned_decision_points_within_range, 
                          by = join_by(mars_id == mars_id, decision_point == decision_point))
dat_analysis <- dat_analysis %>% select(all_of(new_cols), everything())

lookup <- c(counts_rand_past24hrs = "counts_rand_past24hrs")
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: affect items
################################################################################
column_names_affect_items <- c("Q5_response",  # Q5 -- ashamed
                               "Q7_response",  # Q7 -- guilty
                               "Q8_response")  # Q8 -- happy

lookup <- c(ashamed = "Q5_response_cleaned",
            guilty = "Q7_response_cleaned",
            happy = "Q8_response_cleaned")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_affect_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Extremely" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
         )

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: self-regulatory capacity
################################################################################
column_names_src_items <- c("Q46_response",  # Q46 -- self-regulatory capacity item 1
                            "Q47_response")  # Q47 -- self-regulatory capacity item 2

lookup <- c(src1 = "Q46_response_cleaned",
            src2 = "Q47_response_cleaned",
            src_scored = "src_scored")

dat_analysis <- dat_analysis %>%
  mutate(across(.cols = all_of(column_names_src_items), 
                .fns = ~ case_when(
                  .x == "0 Not at all" ~ 0,
                  .x == "1" ~ 1,
                  .x == "2" ~ 2,
                  .x == "3" ~ 3,
                  .x == "4 Very much" ~ 4,
                  .default = NULL
                ),
                .names = "{.col}_cleaned")
  )

dat_analysis <- dat_analysis %>% mutate(src_scored = (Q46_response_cleaned + Q47_response_cleaned)/2)
dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: aggregate measure over the past 24 hours
# For affect items, the aggregate measure is the mean
################################################################################
lookup <- c(ashamed_mean_past24hrs = "ashamed_mean_past24hrs",
            guilty_mean_past24hrs = "guilty_mean_past24hrs",
            happy_mean_past24hrs = "happy_mean_past24hrs",
            src_scored_mean_past24hrs = "src_scored_mean_past24hrs")

list_all_dat <- list()
dat_analysis[["ashamed_mean_past24hrs"]] <- NA
dat_analysis[["guilty_mean_past24hrs"]] <- NA
dat_analysis[["happy_mean_past24hrs"]] <- NA
dat_analysis[["src_scored_mean_past24hrs"]] <- NA

dat_analysis[["ashamed_nreported_past24hrs"]] <- NA
dat_analysis[["guilty_nreported_past24hrs"]] <- NA
dat_analysis[["happy_nreported_past24hrs"]] <- NA
dat_analysis[["src_scored_nreported_past24hrs"]] <- NA

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[j,"ts_coinflip_mountain"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      n_rand_past24hrs <- dat_current_participant[j,"counts_rand_past24hrs"]
      
      if(n_rand_past24hrs > 0){
        dp_within_range <- dat_current_participant[j,"decision_points_past24hrs"][[1]]
        dat_within_range <- dat_current_participant %>% filter(decision_point %in% dp_within_range)
        
        dat_current_participant[j,"ashamed_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["ashamed"]]))
        dat_current_participant[j,"guilty_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["guilty"]]))
        dat_current_participant[j,"happy_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["happy"]]))
        dat_current_participant[j,"src_scored_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["src_scored"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"ashamed_nreported_past24hrs"]){
          dat_current_participant[j,"ashamed_mean_past24hrs"] <- mean(dat_within_range[["ashamed"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"guilty_nreported_past24hrs"]){
          dat_current_participant[j,"guilty_mean_past24hrs"] <- mean(dat_within_range[["guilty"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"happy_nreported_past24hrs"]){
          dat_current_participant[j,"happy_mean_past24hrs"] <- mean(dat_within_range[["happy"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"src_scored_nreported_past24hrs"]){
          dat_current_participant[j,"src_scored_mean_past24hrs"] <- mean(dat_within_range[["src_scored"]], na.rm = TRUE)
        }
        
      }  # This if-then statement only executes if a block had any micro-randomization in the past 24 hours PRIOR TO the current micro-randomization
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: stressor
################################################################################
lookup <- c(stressor_is_none = "Q31_response_cleaned_none",
            stressor_is_family_or_other_relationships = "Q31_response_cleaned_family_or_other_relationships",
            stressor_is_work_or_school = "Q31_response_cleaned_work_or_school",
            stressor_is_financial = "Q31_response_cleaned_financial",
            stressor_is_housing_or_where_you_live = "Q31_response_cleaned_housing_or_where_you_live",
            stressor_is_health = "Q31_response_cleaned_health",
            stressor_is_any = "Q31_response_cleaned_any")

dat_analysis <- dat_analysis %>%
  mutate(Q31_response_cleaned_none = 1*grepl(pattern = "None", x = Q31_response, fixed = TRUE),
         Q31_response_cleaned_family_or_other_relationships = 1*grepl(pattern = "Family/other relationships", x = Q31_response, fixed = TRUE),
         Q31_response_cleaned_work_or_school = 1*grepl(pattern = "Work/School", x = Q31_response, fixed = TRUE),
         Q31_response_cleaned_financial = 1*grepl(pattern = "Financial", x = Q31_response, fixed = TRUE),
         Q31_response_cleaned_housing_or_where_you_live = 1*grepl(pattern = "Housing/Where you live", x = Q31_response, fixed = TRUE),
         Q31_response_cleaned_health = 1*grepl(pattern = "Health", x = Q31_response, fixed = TRUE)) %>%
  mutate(Q31_response_cleaned_none = replace(Q31_response_cleaned_none, is.na(Q31_response), NA),
         Q31_response_cleaned_family_or_other_relationships = replace(Q31_response_cleaned_family_or_other_relationships, is.na(Q31_response), NA),
         Q31_response_cleaned_work_or_school = replace(Q31_response_cleaned_work_or_school, is.na(Q31_response), NA),
         Q31_response_cleaned_financial = replace(Q31_response_cleaned_financial, is.na(Q31_response), NA),
         Q31_response_cleaned_housing_or_where_you_live = replace(Q31_response_cleaned_housing_or_where_you_live, is.na(Q31_response), NA),
         Q31_response_cleaned_health = replace(Q31_response_cleaned_health, is.na(Q31_response), NA))

dat_analysis <- dat_analysis %>%
  mutate(Q31_response_cleaned_any = abs(Q31_response_cleaned_none - 1))

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: substance use, cigarette counts, 
# alcohol servings
################################################################################
lookup <- c(substance_is_none = "Q19_response_cleaned_none",
            substance_is_cigarettes = "Q19_response_cleaned_cigarettes",
            substance_is_alcohol = "Q19_response_cleaned_alcohol",
            substance_is_vape_juul_or_ecigarettes = "Q19_response_cleaned_vape_juul_or_ecigarettes",
            substance_is_cigars_cigarillos_or_little_cigars = "Q19_response_cleaned_cigars_cigarillos_or_little_cigars",
            substance_is_smokeless_tobacco_snus_or_dissolvable_tobacco = "Q19_response_cleaned_smokeless_tobacco_snus_or_dissolvable_tobacco",
            substance_is_marijuana_or_cannabis = "Q19_response_cleaned_marijuana_or_cannabis",
            substance_is_any_nicotine = "Q19_response_cleaned_any_nicotine",
            substance_is_any = "Q19_response_cleaned_any",
            cigarette_counts = "Q20_response_cleaned",
            alcohol_counts = "Q30_response_cleaned",
            cigarette_counts_categorical = "Q20_response_cleaned_categorical",
            alcohol_counts_categorical = "Q30_response_cleaned_categorical")

dat_analysis <- clean_response_substance_use_multiple_select(dat_analysis)
dat_analysis[["Q20_response_cleaned"]] <- clean_response_cigarette_counts(dat_analysis)

dat_analysis <- dat_analysis %>%
  mutate(Q20_response_cleaned_categorical = case_when(
    (Q20_response_cleaned == 0) ~ "no",
    (Q20_response_cleaned > 0) & (Q20_response_cleaned <= 1) ~ "yes - less than one or one",
    (Q20_response_cleaned >= 2) ~ "yes - two or more",
    .default = NULL
  ))

dat_analysis <- dat_analysis %>% 
  mutate(Q30_response_cleaned = case_when(
         Q30_response == "0 (I did not drink)" ~ 0,
         Q30_response == "Less than 1" ~ 0.5,
         Q30_response == "1" ~ 1,
         Q30_response == "2" ~ 2,
         Q30_response == "3" ~ 3,
         Q30_response == "4" ~ 4,
         Q30_response == "5" ~ 5,
         Q30_response == "6" ~ 6,
         Q30_response == "7" ~ 7,
         Q30_response == "8" ~ 8,
         Q30_response == "9" ~ 9,
         Q30_response == "10" ~ 10,
         Q30_response == "More than 10" ~ 11,
         .default = NULL)) %>%
  mutate(Q30_response_cleaned = replace(Q30_response_cleaned, Q19_response_cleaned_alcohol == 0, 0))

dat_analysis <- dat_analysis %>%
  mutate(Q30_response_cleaned_categorical = case_when(
    (Q30_response_cleaned == 0) ~ "no",
    (Q30_response_cleaned > 0) & (Q30_response_cleaned <= 1) ~ "yes - less than one or one",
    (Q30_response_cleaned >= 2) ~ "yes - two or more",
    .default = NULL
  ))

dat_analysis <- rename(dat_analysis, all_of(lookup))
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Create time-varying moderators: aggregate measure over the past 24 hours
# For cigarette and alcohol count items, the aggregate measure is the sum
################################################################################
lookup <- c(stressor_is_any_sum_past24hrs = "stressor_is_any_sum_past24hrs",
            cigarette_counts_sum_past24hrs = "cigarette_counts_sum_past24hrs",
            alcohol_counts_sum_past24hrs = "alcohol_counts_sum_past24hrs",
            substance_is_any_sum_past24hrs = "substance_is_any_sum_past24hrs",
            substance_is_any_nicotine_sum_past24hrs = "substance_is_any_nicotine_sum_past24hrs",
            substance_is_marijuana_or_cannabis_sum_past24hrs = "substance_is_marijuana_or_cannabis_sum_past24hrs",
            substance_is_alcohol_sum_past24hrs = "substance_is_alcohol_sum_past24hrs")

list_all_dat <- list()
dat_analysis[["stressor_is_any_sum_past24hrs"]] <- NA
dat_analysis[["cigarette_counts_sum_past24hrs"]] <- NA
dat_analysis[["alcohol_counts_sum_past24hrs"]] <- NA
dat_analysis[["substance_is_any_sum_past24hrs"]] <- NA
dat_analysis[["substance_is_any_nicotine_sum_past24hrs"]] <- NA
dat_analysis[["substance_is_marijuana_or_cannabis_sum_past24hrs"]] <- NA
dat_analysis[["substance_is_alcohol_sum_past24hrs"]] <- NA

dat_analysis[["stressor_is_any_nreported_past24hrs"]] <- NA
dat_analysis[["cigarette_counts_nreported_past24hrs"]] <- NA
dat_analysis[["alcohol_counts_nreported_past24hrs"]] <- NA
dat_analysis[["substance_is_any_nreported_past24hrs"]] <- NA
dat_analysis[["substance_is_any_nicotine_nreported_past24hrs"]] <- NA
dat_analysis[["substance_is_marijuana_or_cannabis_nreported_past24hrs"]] <- NA
dat_analysis[["substance_is_alcohol_nreported_past24hrs"]] <- NA

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_analysis %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[j,"ts_coinflip_mountain"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      n_rand_past24hrs <- dat_current_participant[j,"counts_rand_past24hrs"]
      
      if(n_rand_past24hrs > 0){
        dp_within_range <- dat_current_participant[j,"decision_points_past24hrs"][[1]]
        dat_within_range <- dat_current_participant %>% filter(decision_point %in% dp_within_range)
        
        dat_current_participant[j,"stressor_is_any_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["stressor_is_any"]]))
        dat_current_participant[j,"cigarette_counts_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["cigarette_counts"]]))
        dat_current_participant[j,"alcohol_counts_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["alcohol_counts"]]))
        dat_current_participant[j,"substance_is_any_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["substance_is_any"]]))
        dat_current_participant[j,"substance_is_any_nicotine_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["substance_is_any_nicotine"]]))
        dat_current_participant[j,"substance_is_marijuana_or_cannabis_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["substance_is_marijuana_or_cannabis"]]))
        dat_current_participant[j,"substance_is_alcohol_nreported_past24hrs"] <- sum(!is.na(dat_within_range[["substance_is_alcohol"]]))
        
        # Note that checking whether number of micro-randomizations is equal to
        # the number of completed EMA will not count partially completed EMAs; 
        # although more verbose to implement in code, 
        # checking each item one at a time is more accurate
        if(n_rand_past24hrs == dat_current_participant[j,"stressor_is_any_nreported_past24hrs"]){
          dat_current_participant[j,"stressor_is_any_sum_past24hrs"] <- sum(dat_within_range[["stressor_is_any"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"cigarette_counts_nreported_past24hrs"]){
          dat_current_participant[j,"cigarette_counts_sum_past24hrs"] <- sum(dat_within_range[["cigarette_counts"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"alcohol_counts_nreported_past24hrs"]){
          dat_current_participant[j,"alcohol_counts_sum_past24hrs"] <- sum(dat_within_range[["alcohol_counts"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"substance_is_any_nreported_past24hrs"]){
          dat_current_participant[j,"substance_is_any_sum_past24hrs"] <- sum(dat_within_range[["substance_is_any"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"substance_is_any_nicotine_nreported_past24hrs"]){
          dat_current_participant[j,"substance_is_any_nicotine_sum_past24hrs"] <- sum(dat_within_range[["substance_is_any_nicotine"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"substance_is_marijuana_or_cannabis_nreported_past24hrs"]){
          dat_current_participant[j,"substance_is_marijuana_or_cannabis_sum_past24hrs"] <- sum(dat_within_range[["substance_is_marijuana_or_cannabis"]], na.rm = TRUE)
        }
        
        if(n_rand_past24hrs == dat_current_participant[j,"substance_is_alcohol_nreported_past24hrs"]){
          dat_current_participant[j,"substance_is_alcohol_sum_past24hrs"] <- sum(dat_within_range[["substance_is_alcohol"]], na.rm = TRUE)
        }
        
      }  # This if-then statement only executes if a block had any micro-randomization in the past 24 hours PRIOR TO the current micro-randomization
    }  # This if-then statement only executes if a block had a micro-randomization
  } # This loop completes checking each participant-block
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_analysis <- bind_rows(list_all_dat)
keep_these_columns_for_analysis <- append(keep_these_columns_for_analysis, list(names(lookup)))

################################################################################
# Select only the columns you will need
################################################################################
keep_these_columns_for_analysis <- unlist(keep_these_columns_for_analysis)
dat_analysis <- dat_analysis %>% select(mars_id, decision_point, all_of(keep_these_columns_for_analysis))
saveRDS(dat_analysis, file = file.path(path_manipulated_data, "dat_mars_time_varying_moderators.rds"))

