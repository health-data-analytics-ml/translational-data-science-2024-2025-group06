# Load libraries, data =========================================================
rm(list = ls())
library(ggplot2)
library(MASS)
library(mclust)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
cluster_assignments <- as.factor(gmm_model$classification)
ukb_cluster_encoded$cluster <- cluster_assignments
# selected_colors <- c("#999999", "#9370DB", "#4DAF4A", "#F781BF", "#FFFF33", "#377EB8", "#A65628") #"#FF7F00", "#984EA3")
selected_colors <- c("#9370DBE6", "#B3B3B3CC", "#4DAF4AE6", "#F781BFCC", "#FFFF33", "#377EB8D9", "#A65628E6")
## Added hex codes at the end to make the colors more muted (CC: 80% opacity)

# Plot of number of people in each cluster =====================================
p <- ggplot(ukb_cluster_encoded, aes(x= factor(cluster), fill = factor(cluster))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  theme(legend.position = '2none',
        plot.title = element_text(hjust = 0.5, size = 25)) +
  labs(title = 'Cluster Membership Counts',
       x = 'Cluster',
       y = 'Count')
p
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Descriptions/num_in_cluster.png', plot = p, width = 8, height = 8, dpi = 300)

# UMAP Plot ====================================================================
## Read in UMAP results
umap_results <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/umap.rds')
umap_df <- as.data.frame(umap_results$layout)
colnames(umap_df) <- c('UMAP1', 'UMAP2')
umap_df$cluster <- cluster_assignments
umap_df <- na.omit(umap_df) # Rows are removed anyway when plotting

## Plot
dev.off()
p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(alpha = .75, size = .1) +
  scale_color_manual(values = selected_colors) +
  theme_minimal() +
  labs(title = 'UMAP Visualisation of GMM Clusters', color = 'Cluster') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_umap_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)


# PC Plot ======================================================================
## Read in PCA results
pca_result <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_result.rds')
pca_scores <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_scores.rds')
pca_scores$cluster <- cluster_assignments

## Scree plot and save as png
dev.off()
png('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gauassian_Mixture_Model/Dimensionality_Reduction_Projections/pca_scree.png', width = 800, height = 600)
ev = with(pca_result, sdev^2/sum(sdev^2))
plot(ev, pch = 19, col = "navy", xlab = "# of PCs",
     ylab = "Proportion of EV", ylim = c(0, 1.2), cex = 0.3,
     main = 'Scree Plot of PCA')
points(cumsum(ev), pch = 19, col = "tomato", cex = 0.3)
legend("top", pch = 19, col = c("navy", "tomato"),
       legend = c("EV", "Cumulative EV"), cex = 0.9, horiz = T)
dev.off()

## Calculate explained variance
ev <- with(pca_result, sdev^2 / sum(sdev^2))
pc1_var <- round(ev[1] * 100, 1)
pc2_var <- round(ev[2] * 100, 1)

## Plot results on first two PCs
par(mar = c(4, 4, 2, 2))
p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = .8, size = .2) +
  scale_color_manual(values = selected_colors) + 
  labs(title = "PCA Visualisation of GMM Clusters",
       x = paste0("PC1 (", pc1_var, "%)"),
       y = paste0("PC2 (", pc2_var, "%)"),
       color = "Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_pca_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)

# t-SNE Plot ===================================================================
## Read in t-SNE results
tsne_result <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/tsne.rds')
tsne_df <- data.frame(X = tsne_result$Y[,1], 
                      Y = tsne_result$Y[,2], 
                      Cluster = ukb_cluster_encoded$cluster)

## Plot
tsne_df$cluster <- as.factor(tsne_df$Cluster)
p <- ggplot(tsne_df, aes(x = X, y = Y, color = cluster)) +
  geom_point(size = .2, alpha = 0.7) +
  scale_color_manual(values = selected_colors) +
  labs(title = "t-SNE Visualisation of GMM Clusters", x = "t-SNE 1", y = "t-SNE 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_tsne_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)


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
  if (nrow(subset_data) > 2) {  ## kde2d requires at least 3 points
    dens <- kde2d(subset_data$PC1, subset_data$PC2, n = 100, lims = c(xlim, ylim))
    ## Convert density output to data frame for ggplot
    dens_df <- expand.grid(x = dens$x, y = dens$y)
    dens_df$z <- as.vector(dens$z)
    dens_df$cluster <- as.factor(cl)  # Assign cluster label
    ## Store density data
    density_list[[as.character(cl)]] <- dens_df
  }
}

## Combine all density data
density_df <- do.call(rbind, density_list)

## Create ggplot with different contour colors for each cluster
density_df$cluster <- factor(density_df$cluster, levels = as.character(1:7))
p <- ggplot(density_df, aes(x = x, y = y, z = z, color = cluster)) +
  geom_contour(bins = 5, size = .55) +
  labs(title = "Cluster Density Contours (PCA)",
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    color = "Cluster") +
  scale_color_manual(
    values = selected_colors,  
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p
 
## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_pca_contour.png', plot = p, width = 8, height = 6, dpi = 300)


# Density map PCA with scatterplot =============================================
dev.off()
# Create ggplot with scatter points and density contours
p <- ggplot() +
  ## Scatter plot of PCA scores, colored by cluster
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = cluster), 
             size = 0.4, alpha = 0.4, shape = 16) +
  ## Density contour lines for each cluster, drawn on top of points
  geom_contour(data = density_df, aes(x = x, y = y, z = z, color = cluster), 
               bins = 5, size = .6) +
  ## Axis labels, title, and manual color scale
  labs(title = "Cluster Density Contours and Scatterplot (PCA)",
       x = paste0("PC1 (", pc1_var, "%)"),
       y = paste0("PC2 (", pc2_var, "%)"),
       color = "Cluster") +
  scale_color_manual(
    values = selected_colors, 
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_pca_contour_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)


# Density map UMAP (all clusters) ==============================================
dev.off()
## Make sure all density estimations have the same x limit and y limit
xlim <- range(umap_df$UMAP1, na.rm = TRUE)
ylim <- range(umap_df$UMAP2, na.rm = TRUE)

## Store densities
density_list <- list()
## Compute density separately for each cluster
for (cl in unique(umap_df$cluster)) {
  subset_data <- subset(umap_df, cluster == cl)
  dens <- kde2d(subset_data$UMAP1, subset_data$UMAP2, n = 100, lims = c(xlim, ylim))
  # Convert density output to data frame for ggplot
  dens_df <- expand.grid(x = dens$x, y = dens$y)
  dens_df$z <- as.vector(dens$z)
  dens_df$cluster <- as.factor(cl)
  # Store density data
  density_list[[as.character(cl)]] <- dens_df
}

## Combine all density data
density_df <- do.call(rbind, density_list)
density_df$cluster <- factor(density_df$cluster, levels = as.character(1:7))
p <- ggplot() +  
  geom_contour(data = density_df, aes(x = x, y = y, z = z, color = cluster), 
               bins = 50, size = 0.5) +  
  scale_alpha_manual(values = c('1'=1, '2'=0.2, '3'=1, '4'=1, '5'=1, '6'=1, '7'=1)) +
  labs(title = "Cluster Density Contours (UMAP)", 
       x = "UMAP1", y = "UMAP2", color = "Cluster") +
  scale_color_manual(values = selected_colors) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_umap_contour.png', plot = p, width = 8, height = 6, dpi = 300)


# Density map and scatterplot UMAP (all clusters) ==============================
p <- ggplot() +  
  geom_point(data = umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(cluster)), 
             size = .4, alpha = 0.35, shape = 16) +
  geom_contour(data = density_df, aes(x = x, y = y, z = z, color = cluster), 
               bins = 50, size = 0.5) +  
  labs(title = "UMAP Visualisation of GMM Clusters", 
       x = "UMAP1", y = "UMAP2", color = "Cluster") +
  scale_color_manual(values = selected_colors) +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 25))
p

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Dimensionality_Reduction_Projections/gmm_umap_contour_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)


# TESTING PCA BEFORE GMM =========================================================
# Recreate model with PCs accounting for >90% of variance
cumulative_ev <- cumsum(ev)
num_pcs <- which(cumulative_ev >= 0.90)[1]  # first index where cumulative variance ≥ 90%
num_pcs # 29 PCs, originally 44 variables
selected_pcs <- pca_result$x[, 1:num_pcs]

gmm_model <- Mclust(selected_pcs)
cluster_assignments <- gmm_model$classification
## UMAP and PC graphs are worse than without running PCA before

# Recreate model with PCs accounting for >75% of variance
cumulative_ev <- cumsum(ev)
num_pcs <- which(cumulative_ev >= 0.75)[1]  # first index where cumulative variance ≥ 75%
num_pcs # 22 PCs, originally 44 variables
selected_pcs <- pca_result$x[, 1:num_pcs]

gmm_model <- Mclust(selected_pcs)
cluster_assignments <- gmm_model$classification
## UMAP and PC graphs are worse than without running PCA before



