library(ggplot2)

setwd('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model')
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores_2.RData')
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/silhouette_scores_2.RData')

k_range <- 4:20
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
bic_plot
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Scoring/bic.png', 
       plot = bic_plot, width = 8, height = 6, dpi = 300)

silhouette_df <- data.frame(Clusters = k_range, Silhouette_Score = silhouette_scores)
# Plot Silhouette Scores
silhouette_plot <- ggplot(silhouette_df, aes(x = Clusters, y = Silhouette_Score)) +
  geom_line(color = "red", size = 1) +  # Line plot
  geom_point(color = "red", size = 3) +  # Dots for each score
  labs(title = "Silhouette Score vs. Number of Clusters",
       x = "Number of Clusters",
       y = "Silhouette Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

silhouette_plot

ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Scoring/silhouette.png', 
       plot = silhouette_plot, width = 8, height = 6, dpi = 300)
