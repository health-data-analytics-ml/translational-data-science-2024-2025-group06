# ------------------------------------------------------------------------------
# Purpose: This script attempts KNN imputation on the dataset (not used as final
# imputation technique)
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(tidyverse)
library(VIM)
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

# Knn imputation ===============================================================
data_imp <- kNN(data, k = 10, imp_var = FALSE)
saveRDS(data_imp, file = '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')
