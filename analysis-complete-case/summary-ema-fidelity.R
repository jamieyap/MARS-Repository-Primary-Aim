rm(list = ls())

###############################################################################
# Input arguments to this script
###############################################################################
source(file = file.path("analysis-multiple-imputation", "mi-set-up.R"))
rm(list = ls())

###############################################################################
# Load packages
###############################################################################
source("paths.R")
library(tidyverse)
# Note that dplyr::select clashes with MASS::select and so we have this line to be
# able to use the select function from the dplyr package while having MASS loaded too
select <- dplyr::select 

###############################################################################
# Calculate aggregate summary statistics about 2-question survey and EMA
###############################################################################
# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

dat_summary_2qs <- dat_primary_aim %>%
  filter(!is.na(status_survey_2qs_collapsed)) %>%
  summarise(n_delivered = n(),
            n_completed = sum(status_survey_2qs_collapsed == "fully_completed"))

print(dat_summary_2qs)

# n_delivered n_completed
# 1        4752        2353

dat_summary_ema <- dat_primary_aim %>%
  filter(!is.na(status_survey_ema_collapsed)) %>%
  summarise(n_delivered = n(),
            n_completed = sum(status_survey_ema_collapsed == "fully_completed"))

print(dat_summary_ema)

# n_delivered n_completed
# 1        4745        2434

###############################################################################
# Calculate summary statistics about EMA for each participant
###############################################################################
# This is the data frame that is the output of the very last step of our data pipeline
dat_primary_aim <- readRDS(file = file.path(path_manipulated_data, "dat_primary_aim.rds")) 
dat_primary_aim <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))
dat_primary_aim_elig <- dat_primary_aim %>% filter(eligibility == 1)

dat_summary_ema <- dat_primary_aim_elig  %>%
  group_by(participant_id) %>%
  summarise(n_elig = sum(eligibility),
            n_complete = sum(status_survey_ema_collapsed == "fully_completed", na.rm = TRUE)) %>%
  mutate(percent_complete = n_complete/n_elig)

dat_summary_ema <- dat_summary_ema %>% arrange(desc(percent_complete))
dat_summary_ema$plot_id <- 1:nrow(dat_summary_ema)

###############################################################################
# Plot
###############################################################################
par(mar=c(6, 6, 6, 6) + 0.1)

plot("n", 
     xlim = c(0,100), 
     ylim = c(0,1), 
     axes = FALSE,
     xlab = "", 
     ylab = "")
lines(dat_summary_ema$plot_id, dat_summary_ema$percent_complete, type = "o", lwd = 3, col = "cornflowerblue", pch = 1, cex = 2)
axis(1, at = c(0,20,40,60,80,100), las = 1, lwd = 4, cex.axis = 2, cex.lab = 2)
axis(2, at = c(0, 0.20, 0.40, 0.60, 0.80, 1), las = 1, 
     lwd = 4, cex.axis = 2, cex.lab = 2,
     col = "cornflowerblue", col.axis = "cornflowerblue")

mtext("Participant",
      side = 1,
      col = "black",
      line = 3.5,
      cex = 2)

mtext("Percent EMA Completed",
      side = 2,
      col = "cornflowerblue",
      line = 4.5,
      cex = 2)

par(new=TRUE)

plot(dat_summary_ema$plot_id, dat_summary_ema$n_complete, 
     type = "o", lwd = 3, col = "red", pch = 15, cex = 2,
     axes = FALSE,
     xlab = "",
     ylab = "",
     ylim = c(0,50))
axis(4, at = c(0, 10, 20, 30, 40, 50), las = 1, 
     lwd = 4, cex.axis = 2, cex.lab = 2,
     col = "red", col.axis = "red")

mtext("# EMA completed",
      side = 4,
      col = "red",
      line = 4.5,
      cex = 2)

legend(70,50,
       legend=c("Percent EMA Completed\n= # EMA completed divided by\n# Decision Points Eligible for\nMicro-randomization",
                "# EMA completed"),
       text.col=c("cornflowerblue","red"),pch=c(1,15),col=c("cornflowerblue","red"),
       cex = 1.4)

