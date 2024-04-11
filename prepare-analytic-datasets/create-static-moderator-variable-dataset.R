rm(list = ls())

source("paths.R")
library(tidyverse)

################################################################################
# Load datasets
################################################################################
v1_baseline_quest <- readRDS(file = file.path(path_static_moderators_data, "v1_baseline_quest.rds"))
mars_ids_excluded_from_all_analytic_datasets <- readRDS(file = file.path(path_manipulated_data, "mars_ids_excluded_from_all_analytic_datasets.rds"))

################################################################################
# First, drop data from participants which we will not be using in any further
# analysis.
################################################################################
v1_baseline_quest <- v1_baseline_quest %>% filter(!(mars_id %in% mars_ids_excluded_from_all_analytic_datasets))

################################################################################
# Select only the columns you need
################################################################################
moderators_to_test <- c("srq_mean", # self-regulation: higher scores indicate higher self-regulation abilities
                        "mdes_pos_mean", # mdes: higher scores indicate more intense positive emotions
                        "mdes_neg_mean", # mdes: higher scores indicate more intense negative emotions
                        "uclals_mean", # loneliness: higher scores indicate a higher sense of loneliness
                        "Nicotine_dep", # heaviness of smoking: higher scores indicate higher smoking levels
                        "phqa1_1",  # have you ever drank alcohol (including beer or wine): 1 = yes, 0 = no
                        "phqa_problem",  # 0/1 indicator of whether the individual has a drinking problem: 1 = yes, 0 = no, -99 = phqa1_1 = 0
                        "ecig_1", # do you use any electronic nicotine delivery system like vape pen, JUUL, or ecigarettes: 1 = yes, 0 = no
                        "ecig_2", # during the past 30 days, how many days did you use a vape pen, JUUL, or ecigarette, even one or two puffs? number: min = 0; max = 30
                        "ecig_3", # on days you use a vape pen or JUUL, please estimate how many separate times per day you usually use it. number: min = 0; max = 500
                        "ecig_4",  # how many vape pen, JUUL, or ecigarette puffs on average do you take each time you use it? number: min = 0, max = 100
                        "maas_total", # mindfulness: higher scores indicate higher mindfulness
                        "FFMQ_total",  # mindfulness: higher scores indicate higher overall mindfulness
                        "ffmq_nonjudge",  # mindfulness: higher scores indicate higher non-judging of inner experiences
                        "ffmq_aware",  # mindfulness: higher scores indicate greater acting with awareness
                        "gratitude") # gratitude: higher scores indicate higher gratitude

auxilliary_variables_for_imputation <- c("SE_total", # self-efficacy overall: higher scores indicate greater confidence to not smoke overall
                                         "se_social", # self-efficacy in social situations: higher scores indicate greater confidence to not smoke in social situations
                                         "se_habit", # self-efficacy habitually: higher scores indicate greater confiednce to not smoke habitually 
                                         "se_negaff", # self-efficacy under negative affect: higher scores indicate greater confidence to not smoke under negative affect
                                         "food_security_mean", # food security: higher scores indicate higher food security
                                         "SSSladders", # subjective social status ladder: higher scores indicate lower socio-economic status
                                         "pp1_1", # perception of poverty: higher scores indicate higher perceived poverty
                                         "FinancialStrain", # financial strain: higher scores indicate greater financial strain
                                         "nd_mean", # neighborhood disadvantage: higher scores indicate higher neighborhood disorganization
                                         "isel_total", # social support overall: higher scores indicate greater interpersonal support
                                         "isel_belonging", # social support: higher scores indicate greater interpersonal support
                                         "isel_appraisal", # social support: higher scores indicate greater sense of belonging
                                         "isel_tangible", # social support: higher scores indicate greater tangible interpersonal support
                                         "sni_count", # social network: higher count indicates greater number of high contact roles
                                         "sni_people", # social support: higher count indicates greater number of people in social network
                                         "sni_active") # social support: higher count indicates greater number of embedded or active roles

dat_baseline <- v1_baseline_quest %>%
  select(all_of(c("mars_id", "v1_baseline_quest_complete", moderators_to_test, auxilliary_variables_for_imputation)))

dat_baseline <- dat_baseline %>%
  mutate(phqa_problem = replace(phqa_problem, phqa_problem == -99, 0))  # phqa_problem was skipped if the participant responded "no" to phqa1_1

################################################################################
# Save
################################################################################
saveRDS(dat_baseline, file = file.path(path_manipulated_data, "dat_mars_baseline_moderators.rds"))

