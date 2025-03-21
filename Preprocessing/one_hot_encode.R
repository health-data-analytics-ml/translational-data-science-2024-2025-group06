rm(list = ls())
library(fastDummies)
ukb_cluster_scaled <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_scaled.rds')

# One hot encode final dataset =================================================
factor_cols <- names(ukb_cluster_scaled)[sapply(ukb_cluster_scaled, is.factor)]
ukb_cluster_scaled$ids <- row.names(ukb_cluster_scaled)
ukb_cluster_encoded <- dummy_cols(ukb_cluster_scaled, 
                                  select_columns = factor_cols,
                                  remove_first_dummy = TRUE,
                                  remove_selected_columns = TRUE)

# Add the rownames back in, this is removed by dummy_cols
rownames(ukb_cluster_encoded) <- ukb_cluster_encoded$ids
ukb_cluster_encoded <- ukb_cluster_encoded[,-15]

# Save as RDS file =============================================================
saveRDS(ukb_cluster_encoded, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')






