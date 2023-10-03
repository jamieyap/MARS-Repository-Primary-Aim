rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
v1_baseline_quest <- readRDS(file = file.path(path_static_moderators_data, "v1_baseline_quest.rds"))

################################################################################
# Select only the columns you need
################################################################################
dat_baseline <- v1_baseline_quest %>%
  select(mars_id, v1_baseline_quest_complete,
         srq_mean, # self-regulation: higher scores indicate higher self-regulation abilities
         mdes_pos_mean, # mdes: higher scores indicate more intense positive emotions
         mdes_neg_mean, # mdes: higher scores indicate more intense negative emotions
         uclals_mean, # loneliness: higher scores indicate a higher sense of loneliness
         Nicotine_dep, # heaviness of smoking: higher scores indicate higher smoking levels
         phqa1_1,  # have you ever drank alcohol (including beer or wine): 1 = yes, 0 = no
         phqa_problem,  # 0/1 indicator of whether the individual has a drinking problem: 1 = yes, 0 = no, -99 = phqa1_1 = 0
         ecig_1, # do you use any electronic nicotine delivery system like vape pen, JUUL, or ecigarettes: 1 = yes, 0 = no
         ecig_2, # during the past 30 days, how many days did you use a vape pen, JUUL, or ecigarette, even one or two puffs? number: min = 0; max = 30
         ecig_3, # on days you use a vape pen or JUUL, please estimate how many separate times per day you usually use it. number: min = 0; max = 500
         ecig_4,  # how many vape pen, JUUL, or ecigarette puffs on average do you take each time you use it? number: min = 0, max = 100
         maas_total, # mindfulness: higher scores indicate higher mindfulness
         FFMQ_total,  # mindfulness: higher scores indicate higher overall mindfulness
         ffmq_nonjudge,  # mindfulness: higher scores indicate higher non-judging of inner experiences
         ffmq_aware,  # mindfulness: higher scores indicate greater acting with awareness
         gratitude) # gratitude: higher scores indicate higher gratitude

dat_baseline <- dat_baseline %>%
  mutate(phqa_problem = replace(phqa_problem, phqa_problem == -99, 0))  # phqa_problem was skipped if the participant responded "no" to phqa1_1

################################################################################
# Save
################################################################################
saveRDS(dat_baseline, file = file.path(path_manipulated_data, "dat_mars_baseline_moderators.rds"))

