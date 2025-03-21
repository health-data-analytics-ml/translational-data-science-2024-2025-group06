rm(list = ls())
library(kohonen)
library(mclust)
library(cluster)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
som.model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/som/som_model.rds')

# Inspect the SOM model's diagnostic plots =====================================
plot(som.model, type="counts", shape="straight")
plot(som.model, type="mapping", shape="straight")
plot(som.model, type="changes") 
plot(som.model, type="dist.neighbours", palette.name=grey.colors, shape="straight")
plot(som.model, type="quality", shape="straight")

# Trying to run GMM ============================================================
codes <- as.data.frame(som.model$codes)
som_gmm_model<- Mclust(codes, G=4)
som_gmm_model<- Mclust(codes, G=10)
summary(som_gmm_model)
plot(som_gmm_model, what = "BIC")
## Always clusters everything in the first cluster, with one node in every other cluster

# Trying hierarchical clustering ===============================================
fviz_nbclust(as.data.frame(som.model$codes), kmeans, method = "wss") # +
  # geom_vline(xintercept = 4, linetype = 2)+
  # labs(subtitle = "Elbow method")

## Create model
set.seed(123)
codes <- as.data.frame(som.model$codes)
hc <- cutree(hclust(dist(codes), method="ward.D2"), 4)

## Plot the SOM grid with clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3")[hc], border="white")
add.cluster.boundaries(som.model, hc)

## Mean of each variable for each cluster
som.model$unit.classif  # Maps each row of `ukb_cluster_encoded` to one of the 64 nodes
data_clusters <- hc[som.model$unit.classif]  # Assign each data point to a cluster
View(aggregate(ukb_cluster_encoded, by=list(cluster=data_clusters), mean))

## Visualising each variable distribution for different clusters
codes_matrix <- som.model$codes[[1]] # Extract matrix
plot(som.model, type = "property", property = codes_matrix[, "pack_years"])

## Trying to run model on different number of clusters ========================
set.seed(123)
hc_3 <- cutree(hclust(dist(codes), method="ward.D2"), 3)
hc_4 <- cutree(hclust(dist(codes), method="ward.D2"), 4)
hc_5 <- cutree(hclust(dist(codes), method="ward.D2"), 5)
hc_6 <- cutree(hclust(dist(codes), method="ward.D2"), 6)

## Plotting for 3 clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue")[hc_3], border="white")
add.cluster.boundaries(som.model, hc_3)

## Plotting for 5 clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3")[hc_5], border="white")
add.cluster.boundaries(som.model, hc_5)

## Plotting for 6 clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3", "firebrick3", "mediumturquoise")[hc_6], border="white")
add.cluster.boundaries(som.model, hc_6)


# Trying UMAP -------------------------------------------------------------

# Create a DataFrame linking each node to its cluster
node_to_cluster <- data.frame(Node = 1:nrow(codes),  # SOM node index
                              Cluster = hc_5)

umap_result <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/umap.rds')
umap_df <- data.frame(UMAP1 = umap_result$layout[,1],

data_assignment <- data.frame(  # Sample index
                              AssignedNode = som.model$unit.classif)  # Node assignments

# Merge assigned nodes with their clusters
data_assignment <- merge(data_assignment, node_to_cluster, 
                         by.x = "AssignedNode", by.y = "Node", all.x = TRUE)

umap_df <- data.frame(UMAP1 = umap_result$layout[,1],
                      UMAP2 = umap_result$layout[,2],
                      Cluster = factor(data_assignment$Cluster))

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.1) +
  labs(title = "UMAP Visualization Colored by Cluster Assignments") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# UMAP Plot ====================================================================
# Read in UMAP results
umap_results <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/umap.rds')
umap_df <- as.data.frame(umap_results$layout)
colnames(umap_df) <- c('UMAP1', 'UMAP2')
umap_df$cluster <- cluster_assignments
umap_df <- na.omit(umap_df) # Rows are removed anyway when plotting

# Plot
dev.off()
p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
  geom_point(alpha = .75, size = .1) +
  scale_color_manual(values = selected_colors) +
  theme_minimal() +
  labs(title = 'UMAP Visualisation of GMM Clusters', color = 'Cluster') +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
p

# Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/gmm_umap_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)


# PC Plot ======================================================================
## Read in PCA results
pca_result <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_result.rds')
pca_scores <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_scores.rds')
pca_scores$cluster <- cluster_assignments

## Scree plot
dev.off()
ev = with(pca_result, sdev^2/sum(sdev^2))
plot(ev, pch = 19, col = "navy", xlab = "# of PCs",
     ylab = "Proportion of EV", ylim = c(0, 1.2), cex = 0.3,
     main = 'Scree Plot of PCA')
points(cumsum(ev), pch = 19, col = "tomato", cex = 0.3)
legend("top", pch = 19, col = c("navy", "tomato"),
       legend = c("EV", "Cumulative EV"), cex = 0.9, horiz = T)

# Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/gmm_pca_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)

# Calculate explained variance
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
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))
p

# Save the plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/gmm_pca_scatterplot.png', plot = p, width = 8, height = 6, dpi = 300)



