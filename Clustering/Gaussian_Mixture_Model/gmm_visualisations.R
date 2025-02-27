# Load libraries, read in data =================================================
rm(list = ls())
library(viridis)
library(ggplot2)
library(MASS)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')

# UMAP Plot ====================================================================
dev.off()
umap_results <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/umap.rds')
umap_df <- as.data.frame(umap_results$layout)
colnames(umap_df) <- c('UMAP1', 'UMAP2')
cluster_assignments <- as.factor(gmm_model$classification)
umap_df$cluster <- cluster_assignments
umap_df <- na.omit(umap_df) # Rows are removed anyway when plotting
dev.off()

selected_colors <- c(
  "1" = "#DDCC77", "2" = "#CC6677", "3" = "#88CCEE", "4" = "#999933",
  "5" = "#332288", "6" = "#AA4499", "7" = "#44AA99", "8" = "#E69F00",
  "9" = "#117733", "10" = "#888888"
)
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(alpha = .75, size = .6) +
  scale_color_manual(values = selected_colors) +
  # scale_color_manual(values = selected_colors) +
  # scale_color_viridis_d(option = "cividis") +
  # scale_color_brewer(palette = 'Set1') +
  theme_minimal() +
  labs(title = 'UMAP Visualisation of GMM Clusters', color = 'Cluster') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# PC Plot ======================================================================
dev.off()
## Get first two PCs to plot
pca_result <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_result.rds')
pca_scores <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_scores.rds')
pca_scores$cluster <- cluster_assignments

## Scree plot
ev = with(pca_result, sdev^2/sum(sdev^2))
plot(ev, pch = 19, col = "navy", xlab = "# of PCs",
     ylab = "Proportion of EV", ylim = c(0, 1.2), cex = 0.3)
points(cumsum(ev), pch = 19, col = "tomato", cex = 0.3)
legend("top", pch = 19, col = c("navy", "tomato"),
       legend = c("EV", "Cumulative EV"), cex = 0.9, horiz = T)

## Plot results on first two PCs
dev.off()
par(mar = c(4, 4, 2, 2))
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = .8, size = .7) +
  scale_color_manual(values = selected_colors) + 
  labs(title = "GMM Clustering Results", color = "Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# t-SNE Plot ===================================================================
## tsne_result <- Rtsne(ukb_cluster_encoded, dims = 2, perplexity = 30)
## takes too long to run - need to submit as a job

# Recreate model with PCs accounting for >90% of variance ======================
cumulative_ev <- cumsum(ev)
num_pcs <- which(cumulative_ev >= 0.90)[1]  # first index where cumulative variance ≥ 90%
num_pcs # 29 PCs, originally 44 variables
selected_pcs <- pca_result$x[, 1:num_pcs]

gmm_model <- Mclust(selected_pcs)
cluster_assignments <- gmm_model$classification
## UMAP and PC graphs are worse than without running PCA before

# Recreate model with PCs accounting for >75% of variance ======================
cumulative_ev <- cumsum(ev)
num_pcs <- which(cumulative_ev >= 0.75)[1]  # first index where cumulative variance ≥ 75%
num_pcs # 22 PCs, originally 44 variables
selected_pcs <- pca_result$x[, 1:num_pcs]

gmm_model <- Mclust(selected_pcs)
cluster_assignments <- gmm_model$classification
## UMAP and PC graphs are worse than without running PCA before

# Density map PCA ==============================================================
dev.off()
## Find limits for consistent density estimation across clusters
xlim <- range(pca_scores$PC1, na.rm = TRUE)
ylim <- range(pca_scores$PC2, na.rm = TRUE)

## Create an empty dataframe to store all densities
density_list <- list()

## Compute density separately for each cluster
for (cl in unique(pca_scores$cluster)) {
  subset_data <- subset(pca_scores, cluster == cl)
  
  if (nrow(subset_data) > 2) {  # kde2d requires at least 3 points
    dens <- kde2d(subset_data$PC1, subset_data$PC2, n = 100, lims = c(xlim, ylim))
    
    # Convert density output to data frame for ggplot
    dens_df <- expand.grid(x = dens$x, y = dens$y)
    dens_df$z <- as.vector(dens$z)
    dens_df$cluster <- as.factor(cl)  # Assign cluster label
    
    # Store density data
    density_list[[as.character(cl)]] <- dens_df
  }
}

## Combine all density data
density_df <- do.call(rbind, density_list)

## Create ggplot with different contour colors for each cluster
ggplot(density_df, aes(x = x, y = y, z = z, color = cluster)) +
  geom_contour(bins = 5, size = 1) +  # Contour lines colored by cluster
  labs(title = "Cluster Density Contours", color = "Cluster",
       x = 'PC1', y = 'PC2') +
  scale_color_manual(values = selected_colors) +  # Assign predefined colors to clusters
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Density map PCA with scatterplot =============================================
dev.off()
# Define color scale for clusters (as given)
selected_colors <- c(
  "1" = "#DDCC77", "2" = "#CC6677", "3" = "#88CCEE", "4" = "#999933",
  "5" = "#332288", "6" = "#AA4499", "7" = "#44AA99", "8" = "#E69F00",
  "9" = "#117733", "10" = "#888888"
)

# Determine limits for consistent density estimation across clusters
xlim <- range(pca_scores$PC1, na.rm = TRUE)
ylim <- range(pca_scores$PC2, na.rm = TRUE)

# Compute density data for each cluster
density_list <- list()
for (cl in unique(pca_scores$cluster)) {
  subset_data <- subset(pca_scores, cluster == cl)
  if (nrow(subset_data) > 2) {  # kde2d requires at least 3 points
    dens <- kde2d(subset_data$PC1, subset_data$PC2, n = 100, lims = c(xlim, ylim))
    dens_df <- expand.grid(x = dens$x, y = dens$y)
    dens_df$z <- as.vector(dens$z)
    dens_df$cluster <- as.factor(cl)
    density_list[[as.character(cl)]] <- dens_df
  }
}
density_df <- do.call(rbind, density_list)

# Create ggplot with scatter points and density contours
ggplot() +
  # Scatter plot of PCA scores, colored by cluster
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = cluster), 
             size = 0.5, alpha = 0.3) +
  # Density contour lines for each cluster, drawn on top of points
  geom_contour(data = density_df, aes(x = x, y = y, z = z, color = cluster), 
               bins = 5, size = .75) +
  # Axis labels, title, and manual color scale
  labs(title = "Cluster Density Contours", color = "Cluster", 
       x = "PC1", y = "PC2") +
  scale_color_manual(values = selected_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Density map UMAP =============================================================
dev.off()
## Find limits for consistent density estimation across clusters
xlim <- range(umap_df$UMAP1, na.rm = TRUE)
ylim <- range(umap_df$UMAP2, na.rm = TRUE)

## Create an empty dataframe to store all densities
density_list <- list()

## Compute density separately for each cluster
for (cl in unique(umap_df$cluster)) {
  subset_data <- subset(umap_df, cluster == cl)
  
  if (nrow(subset_data) > 2) {  # kde2d requires at least 3 points
    dens <- kde2d(subset_data$UMAP1, subset_data$UMAP2, n = 100, lims = c(xlim, ylim))
    
    # Convert density output to data frame for ggplot
    dens_df <- expand.grid(x = dens$x, y = dens$y)
    dens_df$z <- as.vector(dens$z)
    dens_df$cluster <- as.factor(cl)  # Assign cluster label
    
    # Store density data
    density_list[[as.character(cl)]] <- dens_df
  }
}

## Combine all density data
density_df <- do.call(rbind, density_list)

## Create ggplot with different contour colors for each cluster
ggplot(density_df, aes(x = x, y = y, z = z, color = cluster)) +
  geom_contour(bins = 5, size = 1) +  # Contour lines colored by cluster
  labs(title = "Cluster Density Contours", color = "Cluster",
       x = 'PC1', y = 'PC2') +
  scale_color_manual(values = selected_colors) +  # Assign predefined colors to clusters
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))











