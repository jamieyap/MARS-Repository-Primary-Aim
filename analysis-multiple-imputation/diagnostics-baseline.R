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

dat_wide_init <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_init_baseline.rds"))
dat_wide_completed <- readRDS(file = file.path(path_multiple_imputation_pipeline_data, "sequentially-completed-datasets", mi_dataset_num, "dat_wide_completed_baseline.rds"))

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

prepare_data_for_plotting <- function(variable_name, data_before_imputation, data_after_imputation){
  
  cc_indicator <- mice::cci(data_before_imputation[, c("participant_id", variable_name)])
  ic_indicator <- mice::ici(data_before_imputation[, c("participant_id", variable_name)])
  
  dat_cc <- data_before_imputation[cc_indicator, c("mi_dataset_number", "participant_id", variable_name)]
  dat_ic <- data_after_imputation[ic_indicator, c("mi_dataset_number", "participant_id", variable_name)]
  
  dat_cc[["is_observed"]] <- "observed"
  dat_ic[["is_observed"]] <- paste("imputed dataset ", unique(dat_ic[["mi_dataset_number"]]))
  
  dat_all <- rbind(dat_cc, dat_ic)
  dat_all[["is_observed"]] <- as_factor(dat_all[["is_observed"]])
  
  return(dat_all)
}

###############################################################################
# income_val
###############################################################################
var_to_check <- "income_val"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 14
use_breaks <- seq(0,14,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_income_val.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# srq_mean
###############################################################################
var_to_check <- "srq_mean"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))

g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_srq_mean.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# mdes_pos_mean
###############################################################################
var_to_check <- "mdes_pos_mean"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_mdes_pos_mean.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# mdes_neg_mean
###############################################################################
var_to_check <- "mdes_neg_mean"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_mdes_neg_mean.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# ffmq_nonjudge
###############################################################################
var_to_check <- "ffmq_nonjudge"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_ffmq_nonjudge.png"),
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# maas_total
###############################################################################
var_to_check <- "maas_total"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_maas_total.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# gratitude
###############################################################################
var_to_check <- "gratitude"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

use_min_limits <- 0
use_max_limits <- 7
use_breaks <- seq(0,7,1)
g <- ggplot(dat_for_plotting, aes(x = dat_for_plotting[["is_observed"]], y = dat_for_plotting[[var_to_check]]), group = "is_observed")
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g + geom_violin(color = "pink", fill = "pink") + stat_boxplot(geom ='errorbar', width = 0.25, coef=NULL, linewidth = 2) + geom_boxplot(width = 0.25, coef = NULL, size = 2) + labs(x = NULL, y = var_to_check) + scale_y_continuous(limits = c(use_min_limits, use_max_limits), breaks = use_breaks)

ggsave(filename = file.path(this_location, "baseline_gratitude.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

###############################################################################
# has_partner
###############################################################################
var_to_check <- "has_partner"

dat_for_plotting <- prepare_data_for_plotting(variable_name = var_to_check, data_before_imputation = dat_wide_init, data_after_imputation = dat_wide_completed)
n_missing <- dat_for_plotting %>% 
  filter(is_observed != "observed") %>%
  nrow(.)
n_observed <- nrow(dat_for_plotting) - n_missing

dat_has_partner <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(has_partner == 1)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)
dat_has_partner[["has_partner"]] <- 1

dat_has_no_partner <- dat_for_plotting %>%
  group_by(is_observed) %>%
  mutate(prop = sum(has_partner == 0)/n()) %>%
  select(is_observed, prop) %>%
  unique(.)
dat_has_no_partner[["has_partner"]] <- 0

dat_for_plotting <- rbind(dat_has_partner, dat_has_no_partner)
dat_for_plotting[["has_partner"]] <- as_factor(dat_for_plotting[["has_partner"]])


g <- ggplot(dat_for_plotting, aes(x = is_observed, y = prop, fill = has_partner))
g <- g + ggtitle(label = paste("No. participants with missing value: ", n_missing, "\nNo. of participants with observed value: ", n_observed))
g_out <- g +
  geom_col(position = position_dodge(), width = 0.5) +
  geom_text(aes(label = paste(round(100 * prop), "%")), position = position_dodge(.5), vjust = -.2) +
  labs(x = NULL, y = "proportion") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.10))

ggsave(filename = file.path(this_location, "baseline_has_partner.png"), 
       plot = g_out,
       height = 10, width = 5, units = "in")

