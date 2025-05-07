# ------------------------------------------------------------------------------
# Purpose: This script scales numeric columns in our dataset
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(tidyverse)
ukb_final_reduced <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')

# Remove sex, age, ethnic background for clustering
ukb_cluster <- ukb_final_reduced %>% select(-'sex', -'age', -'ethnic_background')

# Scale ========================================================================
ukb_cluster_scaled <- ukb_cluster %>% mutate(across(where(~is.double(.)), ~ as.numeric(scale(.))))
str(ukb_cluster_scaled)

# Save as RDS file
saveRDS(ukb_cluster_scaled, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_scaled.rds')
