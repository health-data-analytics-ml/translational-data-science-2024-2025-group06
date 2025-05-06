# Load libraries, data =========================================================
rm(list = ls())
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')

# Get dataframe in correct form ================================================
### variance within each cluster for one variable versus the variance of the variable across the entire dataset

# Create Euclidean distance matrix =============================================
