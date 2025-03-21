# Load libraries, read in data =================================================
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

# Save BIC & Silhouette scores -------------------------------------------------
save(bic_scores, file='/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores_3.RData')
save(silhouette_scores, file='/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/silhouette_scores_3.RData')

# Create BIC table and plot ---------------------------------------------------
bic_table <- data.frame(Clusters = k_range, BIC_Score = bic_scores)
print(head(bic_table))  # Check table structure

# Plot the BIC scores
bic_plot <- ggplot(bic_table, aes(x = Clusters, y = BIC_Score)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "BIC Scores for Different Cluster Counts",
       x = "Number of Clusters",
       y = "BIC Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save BIC plot
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_plot_seed1.png', 
       plot = bic_plot, width = 8, height = 6, dpi = 300)

## Unfortunately BIC decreases with increasing number of clusters, chose 10

# Create Silhouette table and plot ---------------------------------------------
silhouette_df <- data.frame(Clusters = k_range, Silhouette_Score = silhouette_scores)
print(head(silhouette_df))  # Check table structure

# Plot Silhouette Scores
silhouette_plot <- ggplot(silhouette_df, aes(x = Clusters, y = Silhouette_Score)) +
  geom_line(color = "red", size = 1) +  # Line plot
  geom_point(color = "red", size = 3) +  # Dots for each score
  labs(title = "Silhouette Score vs. Number of Clusters",
       x = "Number of Clusters",
       y = "Silhouette Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

# Save Silhouette plot
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/silhouette_plot_seed1.png', 
       plot = silhouette_plot, width = 8, height = 6, dpi = 300)

# Run final model with specified number of clusters ===========================
set.seed(43)
gmm_model <- Mclust(ukb_cluster_encoded, G=10)
cluster_assignments <- gmm_model$classification
saveRDS(gmm_model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')

print("Final GMM Model with 10 Clusters saved successfully!")

## Run final model with specified number of clusters ===========================
# set.seed(43)
# gmm_model <- Mclust(ukb_cluster_encoded, G=10)
# cluster_assignments <- gmm_model$classification
# saveRDS(gmm_model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')








