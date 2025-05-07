# ------------------------------------------------------------------------------
# Purpose: This script plots applies UMAP dimensionality reduction on exposure data
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(umap)
rm(list = ls())
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# Run UMAP =====================================================================
umap_results <- umap(ukb_cluster_encoded)
saveRDS(umap_results, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/umap.rds')
