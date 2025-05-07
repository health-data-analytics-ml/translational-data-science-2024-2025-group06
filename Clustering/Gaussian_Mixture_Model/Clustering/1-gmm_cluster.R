# ------------------------------------------------------------------------------
# Purpose: This script performs clustering based on exposures using GMM with 7 
# clusters- decided using BIC in Optimal_Number_Clusters
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(mclust)
library(dplyr)
library(Rtsne)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# Run clustering model with 7 clusters =========================================
set.seed(1)
gmm_model <- Mclust(ukb_cluster_encoded, G=7)
summary(gmm_model)
cluster_assignments <- gmm_model$classification
saveRDS(gmm_model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
