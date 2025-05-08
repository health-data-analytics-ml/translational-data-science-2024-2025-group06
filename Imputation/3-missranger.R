# ------------------------------------------------------------------------------
# Purpose: This script performs imputation on the dataset using missRanger (used 
# as final imputation technique)
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missRanger))
data <- readRDS('/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

# Impute with MissRanger =======================================================
data_imp <- missRanger(data, seed = 8, maxiter = 3, num.trees = 100, pmm.k = 5)
saveRDS(data_imp, '/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')
