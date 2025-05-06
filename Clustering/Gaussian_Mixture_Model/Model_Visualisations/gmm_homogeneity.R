# Load libraries, read in data =================================================
rm(list = ls())
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
cluster_assignments <- as.factor(gmm_model$classification)

# Calculate WCSS for each cluster ==============================================
wcss <- function(exposure_data, clusters) {
  wcss_list <- numeric(length(unique(clusters)))
  names(wcss_list) <- sort(unique(clusters))
  for (k in unique(clusters)) {
    cluster_points <- exposure_data[clusters == k, ]
    center <- colMeans(cluster_points)
    wcss <- sum(rowSums((cluster_points - center)^2))
    wcss_list[k] <- wcss
  }
  return(wcss_list)
}
wcss_values <- wcss(ukb_cluster_encoded, cluster_assignments)

# Create visualisation for WCSS ================================================
wcss_df <- data.frame(
  Cluster = c('1', '2', '3', '4', '5', '6', '7'),
  WCSS = as.numeric(wcss_values)
)
selected_colors <- c("#9370DBE6", "#B3B3B3CC", "#4DAF4AE6", "#F781BFCC", "#FFFF33", "#377EB8D9", "#A65628E6")
p <- ggplot(wcss_df, aes(x= Cluster, y = WCSS, fill = Cluster)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(WCSS, 1)), vjust = -0.5) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  theme(legend.position = '2none',
        plot.title = element_text(hjust = 0.5, size = 25)) +
  labs(title = 'Within-Cluster Sum of Squares (WCSS)',
       x = 'Cluster',
       y = 'WCSS')
p
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Descriptions/cluster_wcss.png', plot = p, width = 8, height = 8, dpi = 300)








