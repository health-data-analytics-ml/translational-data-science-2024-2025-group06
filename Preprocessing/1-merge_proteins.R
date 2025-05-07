# ------------------------------------------------------------------------------
# Purpose: This script merges exposure data with protein data, only including
# participants who have data for both
# ------------------------------------------------------------------------------

# Data =========================================================================
# Filter ukb_clean for 52,704 participants with protein data
# Takes a while but does run!
proteins <- as.data.frame(readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/Proteins/Data/Proteins_nd_imputed.rds'))
ukb_imputed <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')

# Create a merged dataset of ukb_clean and proteins
ukb_proteins_merge <- merge(ukb_imputed, proteins, by=0, all = FALSE)
ukb_proteins_merge <- data.frame(ukb_proteins_merge, row.names = 1)

# Just exposures
columns <- colnames(ukb_imputed)
ukb_proteins_small <- ukb_proteins_merge[,c(columns)]

saveRDS(ukb_proteins_merge, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_proteins_merge.rds')
saveRDS(ukb_proteins_small, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_proteins_small.rds')
