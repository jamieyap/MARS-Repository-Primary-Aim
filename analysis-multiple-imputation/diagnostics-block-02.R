###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))

###############################################################################
# Load packages and datasets
###############################################################################
rm(list = ls())

source("paths.R")
library(tidyverse)
library(mice)

mi_dataset_num <- .__par_mi_number

dat_wide_init <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_init_block2.rds"))
dat_wide_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_block2.rds"))

###############################################################################
# Directory where plots are saved
###############################################################################

is_dir_exist <- file.exists(file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots"))

if(isFALSE(is_dir_exist)){
  dir.create(file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots"))
}

this_location <- file.path(path_multiple_imputation_pipeline_data, "diagnostic-plots", mi_dataset_num)

is_dir_exist <- file.exists(this_location)

if(isFALSE(is_dir_exist)){
  dir.create(this_location)
}

###############################################################################
# Imputation diagnostics
###############################################################################

prepare_data_for_plotting <- function(variable_name, data_before_imputation, data_after_imputation, eligibility_indicator, subgroup_indicator = NULL){
  
  if(is.null(subgroup_indicator)){
    data_before_imputation <- data_before_imputation %>% filter(.data[[eligibility_indicator]] == 1)
    data_after_imputation <- data_after_imputation %>% filter(.data[[eligibility_indicator]] == 1)
    
    cc_indicator <- mice::cci(data_before_imputation[, c("participant_id", variable_name)])
    ic_indicator <- mice::ici(data_before_imputation[, c("participant_id", variable_name)])
    
    dat_cc <- data_before_imputation[cc_indicator, c("mi_dataset_number", "participant_id", variable_name)]
    dat_ic <- data_after_imputation[ic_indicator, c("mi_dataset_number", "participant_id", variable_name)]
  }else{
    data_before_imputation <- data_before_imputation %>% filter(.data[[eligibility_indicator]] == 1)
    data_after_imputation <- data_after_imputation %>% filter(.data[[eligibility_indicator]] == 1)
    
    data_before_imputation[[subgroup_indicator]] <- as_factor(data_before_imputation[[subgroup_indicator]])
    data_after_imputation[[subgroup_indicator]] <- as_factor(data_after_imputation[[subgroup_indicator]])
    
    cc_indicator <- mice::cci(data_before_imputation[, c("participant_id", variable_name)])
    ic_indicator <- mice::ici(data_before_imputation[, c("participant_id", variable_name)])
    
    dat_cc <- data_before_imputation[cc_indicator, c("mi_dataset_number", "participant_id", variable_name, subgroup_indicator)]
    dat_ic <- data_after_imputation[ic_indicator, c("mi_dataset_number", "participant_id", variable_name, subgroup_indicator)]
  }
  
  dat_cc[["is_observed"]] <- "observed"
  dat_ic[["is_observed"]] <- paste("imputed dataset ", unique(dat_ic[["mi_dataset_number"]]))
  
  dat_all <- rbind(dat_cc, dat_ic)
  dat_all[["is_observed"]] <- as_factor(dat_all[["is_observed"]])
  
  return(dat_all)
}

###############################################################################
# Y_dp2 -- overall
###############################################################################

var_to_check <- "Y_dp2"
dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed, eligibility_indicator = "eligibility_dp2")

dat_yes <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(.data[[var_to_check]] == 1)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_yes[[var_to_check]] <- 1

dat_no <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(.data[[var_to_check]] == 0)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_no[[var_to_check]] <- 0

dat_for_plotting <- rbind(dat_yes, dat_no)
dat_for_plotting[[var_to_check]] <- as_factor(dat_for_plotting[[var_to_check]])

g_out <- ggplot(dat_for_plotting, aes(x = is_observed, y = prop, fill = .data[[var_to_check]])) +
  geom_col(position = position_dodge(), width = 0.5) +
  geom_text(aes(label = paste(round(100 * prop), "%")), position = position_dodge(.5), vjust = -.2) +
  labs(x = NULL, y = "proportion") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.10))

ggsave(filename = file.path(this_location, "Y_dp2.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# Y_dp2 -- by subgroup
###############################################################################

var_to_check <- "Y_dp2"
use_subgroup_indicator <- "coinflip_dp2"

subgroup_names <- c(
  `0` = "No Prompt",
  `1` = "Prompt (any kind)"
)

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed, eligibility_indicator = "eligibility_dp2", subgroup_indicator = use_subgroup_indicator)

dat_yes <- dat_for_plotting %>%
  group_by(is_observed, .data[[use_subgroup_indicator]]) %>%
  mutate(prop = sum(.data[[var_to_check]] == 1)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_yes[[var_to_check]] <- 1

dat_no <- dat_for_plotting %>%
  group_by(is_observed, .data[[use_subgroup_indicator]]) %>%
  mutate(prop = sum(.data[[var_to_check]] == 0)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_no[[var_to_check]] <- 0

dat_for_plotting <- rbind(dat_yes, dat_no)
dat_for_plotting[[var_to_check]] <- as_factor(dat_for_plotting[[var_to_check]])

g_out <- ggplot(dat_for_plotting, aes(x = is_observed, y = prop, fill = .data[[var_to_check]])) +
  geom_col(position = position_dodge(), width = 0.5) +
  geom_text(aes(label = paste(round(100 * prop), "%")), position = position_dodge(.5), vjust = -.2) +
  labs(x = NULL, y = "proportion") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.10)) +
  facet_grid(.data[[use_subgroup_indicator]] ~., labeller = as_labeller(subgroup_names))

ggsave(filename = file.path(this_location, "Y_dp2_by_trt.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# quick_survey_response_dp2 -- overall
###############################################################################

var_to_check <- "quick_survey_response_dp2"
dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed, eligibility_indicator = "eligibility_dp2")

dat_1 <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(.data[[var_to_check]] == 1)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_1[[var_to_check]] <- 1

dat_2 <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(.data[[var_to_check]] == 0)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)

dat_2[[var_to_check]] <- 0

dat_for_plotting <- rbind(dat_1, dat_2)
dat_for_plotting[[var_to_check]] <- as_factor(dat_for_plotting[[var_to_check]])

g_out <- ggplot(dat_for_plotting, aes(x = is_observed, y = prop, fill = .data[[var_to_check]])) +
  geom_col(position = position_dodge(), width = 0.5) +
  geom_text(aes(label = paste(round(100 * prop), "%")), position = position_dodge(.5), vjust = -.2) +
  labs(x = NULL, y = "proportion") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.10))

ggsave(filename = file.path(this_location, "quick_survey_response_dp2.png"), 
       plot = g_out,
       height = 5, width = 10, units = "in")

###############################################################################
# number of cigarettes smoked
###############################################################################

var_to_check <- "cigarette_counts_dp2"
dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed, eligibility_indicator = "eligibility_dp2")

use_min_limits <- 0
use_max_limits <- 12
use_breaks <- seq(0,12,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "cigarette_counts_dp2.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# self-regulatory capacity
###############################################################################

var_to_check <- "src_scored_dp2"
dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed, eligibility_indicator = "eligibility_dp2")

use_min_limits <- 0
use_max_limits <- 5
use_breaks <- seq(0,5,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "src_scored_dp2.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

