rm(list = ls())
library(kohonen)
library(mclust)
library(cluster)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
som.model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/som/som_model')

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
hc_4 <- cutree(hclust(dist(codes), method="ward.D2"), 4)
hc_5 <- cutree(hclust(dist(codes), method="ward.D2"), 5)
hc_6 <- cutree(hclust(dist(codes), method="ward.D2"), 6)

## Plotting for 5 clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3", "firebrick3")[hc_5], border="white")
add.cluster.boundaries(som.model, hc_5)

## Plotting for 6 clusters
plot(som.model, type="mapping", shape="straight", cex=0.3,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3", "firebrick3", "mediumturquoise")[hc_6], border="white")
add.cluster.boundaries(som.model, hc_6)

## Maybe need to submit as a job? taking forever to run
data_clusters <- hc_4[som.model$unit.classif]
ss_4 <- mean(silhouette(data_clusters, dist(ukb_cluster_encoded))[, 3])
data_clusters <- hc_5[som.model$unit.classif]
ss_5 <- mean(silhouette(data_clusters, dist(ukb_cluster_encoded))[, 3])
data_clusters <- hc_6[som.model$unit.classif]
ss_6 <- mean(silhouette(data_clusters, dist(ukb_cluster_encoded))[, 3])


