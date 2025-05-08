# ------------------------------------------------------------------------------
# Purpose: This script attempts imputation on the dataset using missForest (not 
# used as final imputation technique)
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missForest))
data <- readRDS('/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')
data <- data %>% mutate(num_household = as.factor(num_household))

# Impute with MissForest =======================================================
data_imp <- missForest(data)
final_file <- data_imp$ximp
saveRDS(final_file, '/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed_rf.rds')
