# ------------------------------------------------------------------------------
# Purpose: This script calculates BIC and silhouette scores to determine the 
# optimal number of clusters for GMM
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(mclust)
library(dplyr)
library(Rtsne)
library(cluster)
library(ggplot2)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# Creating GMM =================================================================
## Test out 4-20 clusters, look for what number of clusters has the lowest BIC
set.seed(1)
k_range <- 4:20
bic_scores <- numeric(length(k_range))  
silhouette_scores <- numeric(length(k_range))

# Loop through different cluster numbers
for (i in seq_along(k_range)) {
  k <- k_range[i]
  
  # Fit GMM with k clusters
  gmm_model <- Mclust(ukb_cluster_encoded, G = k)
  
  # Store BIC score
  bic_scores[i] <- gmm_model$BIC
  
  # Compute silhouette score only if clustering was successful
  if (!is.null(gmm_model$classification)) {
    # Extract cluster assignments
    clusters <- gmm_model$classification
    
    # Compute distance matrix
    dist_matrix <- dist(ukb_cluster_encoded)
    
    # Compute silhouette score
    sil_scores <- silhouette(clusters, dist_matrix)
    
    # Store mean silhouette score
    silhouette_scores[i] <- mean(sil_scores[, 3])  # 3rd column is silhouette width
  } else {
    silhouette_scores[i] <- NA  # If clustering fails, set NA
  }
}

# Save BIC & Silhouette scores =================================================
save(bic_scores, file='/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/Optimal_Number_Clusters/bic_scores.RData')
save(silhouette_scores, file='/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/Optimal_Number_Clusters/silhouette_scores.RData')
