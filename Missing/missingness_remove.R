# ------------------------------------------------------------------------------
# Purpose: This script removes individuals with 5+ missingn values, and does not
# remove any variables/columns
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_clean.rds')

# Remove rows ==================================================================
rows_full <- nrow(data)
data_cleaned_full <- data[which(rowMeans(!is.na(data)) >= 0.875), ]
rows_cleaned <- nrow(data_cleaned_full)

# Removed 27,857 rows (5.55% of rows)
rows_full - rows_cleaned
(rows_full - rows_cleaned) / rows_full

# Save dataset
saveRDS(data_cleaned_full, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')
