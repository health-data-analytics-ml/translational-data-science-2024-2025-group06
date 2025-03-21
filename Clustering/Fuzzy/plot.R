library(e1071)    
library(ggplot2)  
library(cluster)  
library(fpc)  
library(reshape2)

# ------------------------------
# 1. Data Selection: Extract Variables for Clustering
# ------------------------------
# Here we read the ukb_cluster dataset (for example, the first 500 rows)
cluster_data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster.rds')
#cluster_data <- cluster_data[1:500, ]

# ------------------------------
# 2. Fuzzy C-means Clustering
# ------------------------------
# Parameter details:
#   - centers: number of clusters (e.g., 3 clusters)
#   - m: fuzziness exponent, commonly set to 2
#   - iter.max: maximum number of iterations
set.seed(123)
k <- 3
m <- 2
fuzzy_result <- cmeans(cluster_data, centers = k, iter.max = 100, m = m,
                       verbose = TRUE, method = "cmeans")

# Print the clustering result (cluster centers, membership matrix)
print(fuzzy_result)

# ------------------------------
# 3. Membership Degree Matrix & Centers
# ------------------------------
fuzzy_membership_matrix <- fuzzy_result$membership    # Membership matrix (values in [0,1])
initial_centers <- fuzzy_result$centers               # Initial cluster centers
final_centers <- t(fuzzy_result$centers)              # Transposed cluster centers (if needed)

# ------------------------------
# 4. Convert Fuzzy Membership to Hard Clusters
# ------------------------------
# For each sample, pick the cluster with the highest membership as the hard label
cluster_data$cluster <- apply(fuzzy_membership_matrix, 1, which.max)

# Combine the membership matrix with the original data (optional)
cluster_membership <- as.data.frame(fuzzy_membership_matrix)
data_with_clusters <- cbind(cluster_data, cluster_membership)

# ------------------------------
# 5. PCA on the Original Data (Excluding the 'cluster' column)
# ------------------------------
# Remove the 'cluster' column if present, since it's not a numeric variable for PCA
# Also ensure all variables in 'cluster_data' are numeric except 'cluster'
pca_result <- prcomp(cluster_data[, !colnames(cluster_data) %in% "cluster"], 
                     scale. = FALSE)

# Create a data frame of PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Add the cluster labels from cluster_data to pca_scores
# (both should have the same number of rows in the same order)
pca_scores$cluster <- cluster_data$cluster

# Convert the cluster column to a factor
pca_scores$cluster <- as.factor(pca_scores$cluster)

# ------------------------------
# 6. Calculate Explained Variance
# ------------------------------
ev <- with(pca_result, sdev^2 / sum(sdev^2))
pc1_var <- round(ev[1] * 100, 1)
pc2_var <- round(ev[2] * 100, 1)

# Define colors for the 3 clusters
selected_colors <- c("#4DAF4A", "#E41A1C", "#377EB8")

# ------------------------------
# 7. Plot the PCA Results Using ggplot2
# ------------------------------
p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.8, size = 1) +
  scale_color_manual(values = selected_colors) +
  labs(
    title = "PCA Visualisation of Fuzzy C-means Clusters",
    x = paste0("PC1 (", pc1_var, "% Variance)"),
    y = paste0("PC2 (", pc2_var, "% Variance)"),
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Display the plot
print(p)

# ------------------------------
# 8. Save the PCA Plot
# ------------------------------
ggsave(
  filename = "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/fuzzy_pca_plot.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
