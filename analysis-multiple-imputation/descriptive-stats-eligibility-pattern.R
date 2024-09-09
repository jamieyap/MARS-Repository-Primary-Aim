rm(list = ls())

###############################################################################
# Load packages and datasets
###############################################################################
source("paths.R")
library(tidyverse)
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 

###############################################################################
# Data preparation steps
###############################################################################
dat_primary_aim <- dat_primary_aim %>%
  group_by(participant_id) %>%
  mutate(eligibility_lag1 = lag(eligibility)) %>%
  mutate(eligibility_lag1 = replace(eligibility_lag1, decision_point==1, 0)) %>%
  ungroup(.)

dat_primary_aim <- dat_primary_aim %>%
  mutate(which_pattern = case_when(
    eligibility == 1 & eligibility_lag1 == 1 ~ 1,
    eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 1 ~ 2,
    eligibility == 1 & eligibility_lag1 == 0 & any_recent_eligible_dp == 0 ~ 3,
    eligibility == 0 ~ 4,
    .default = NULL
  ))

dat_primary_aim <- dat_primary_aim %>%
  select(mars_id, participant_id, cluster_id, decision_point,
         eligibility, eligibility_lag1, any_recent_eligible_dp, which_pattern, everything())

###############################################################################
# Create descriptive statistics marginal over decision point
###############################################################################
dat_stats <- dat_primary_aim %>%
  group_by(which_pattern) %>%
  summarise(count = n()) %>%
  mutate(which_pattern_factor = case_when(
    which_pattern == 1 ~ "Participant-decision points eligible at t and were also eligible at t-1",
    which_pattern == 2 ~ "Participant-decision points eligible at t, ineligible at t-1, but had at least one eligible decision point prior to t",
    which_pattern == 3 ~ "Participant-decision points which were eligible at t but had no eligible decision point prior to t since the start of the MRT",
    which_pattern == 4 ~ "Participant-decision points not eligible at t",
    .default = NULL
  )) %>%
  mutate(which_pattern_factor = as_factor(which_pattern_factor)) %>%
  select(which_pattern_factor, which_pattern, count)

write.csv(dat_stats, file = file.path("analysis-multiple-imputation", "formatted-output", "eligibility_pattern_descriptive_stats.csv"))

###############################################################################
# Create descriptive statistics conditionally on decision point
###############################################################################
dat_stats_by_dp <- dat_primary_aim %>%
  group_by(decision_point, which_pattern) %>%
  summarise(count = n())

dat_all <- expand.grid(decision_point = 1:60, which_pattern = 1:4)

dat_stats_by_dp <- full_join(x = dat_all, 
                             y = dat_stats_by_dp, 
                             by = join_by(decision_point == decision_point, 
                                          which_pattern == which_pattern))

dat_stats_by_dp <- dat_stats_by_dp %>% 
  mutate(count = replace(count, is.na(count), 0))

dat_stats_by_dp <- dat_stats_by_dp %>%
  mutate(which_pattern_factor = case_when(
    which_pattern == 1 ~ "Participant-decision points eligible at t\nand were also eligible at t-1",
    which_pattern == 2 ~ "Participant-decision points eligible at t, ineligible at t-1,\nbut had at least one eligible decision point prior to t",
    which_pattern == 3 ~ "Participant-decision points which were eligible at t\nbut had no eligible decision point prior to t since the start of the MRT",
    which_pattern == 4 ~ "Participant-decision points not eligible at t",
    .default = NULL
  )) %>%
  mutate(which_pattern_factor = as_factor(which_pattern_factor))

ggplot(dat_stats_by_dp, aes(x = decision_point, y = count, color = which_pattern_factor)) +
  geom_point(size = 3) + geom_line(linewidth = 1) +
  scale_x_continuous(name = "Decision Point", breaks = seq(0,60,6)) +
  scale_y_continuous(name = "Number of participants", limits = c(-2,100), breaks = seq(0,100,10)) +
  theme(axis.text = element_text(size = 18), title = element_text(size = 20), legend.position = "top") +
  guides(col=guide_legend(title="Eligibility Pattern"))

ggsave(filename = file.path("analysis-multiple-imputation", "formatted-output", "eligibility_pattern_descriptive_stats_by_dp.png"), width = 16, height = 8, units = "in", dpi = 1000)

###############################################################################
# Clean up
###############################################################################
if(file.exists("analysis-multiple-imputation/formatted-output/Thumbs.db")){
  file.remove("analysis-multiple-imputation/formatted-output/Thumbs.db")
}
