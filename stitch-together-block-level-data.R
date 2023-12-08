source("paths.R") 
library(tidyverse)

source("check-linkage-algorithm/node-F.R")

saveRDS(dat_output, file = file.path(path_manipulated_data, "dat_all_burst_with_pilot.rds"))
saveRDS(dat_output_without_pilot_participants, file = file.path(path_manipulated_data, "dat_all_burst.rds"))
saveRDS(dat_decisions_atypical_sequences, file = file.path(path_manipulated_data, "dat_decisions_atypical_sequences.rds"))  # Note that this file DOES NOT contain any of the pilot participant IDs
