library(e1071)    
library(ggplot2)  
library(cluster)  
library(fpc)  
library(reshape2) 
# ------------------------------1. Data Selection: Extract Variables for Clustering-------
# Here we read the ukb_cluster dataset (first 1000 rows if desired)
cluster_data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster.rds')
#cluster_data <- ukb_cluster[1:1000, ]
#View(cluster_data)
#str(cluster_data) 

# ------------------------------2. Fuzzy C-means------------------------------
# Parameter details:
#   - centers: number of clusters (e.g., 3 clusters)
#   - m: fuzziness exponent, commonly set to 2
#   - iter.max: maximum number of iterations

set.seed(123)  
k <- 3
m <- 2
fuzzy_result <- cmeans(cluster_data, centers = k, iter.max = 100, m = m, verbose = TRUE, method = "cmeans")

# Print the clustering result, which includes cluster centers and membership for each sample
print(fuzzy_result)
# fuzzy_result$membership #membership matrix
# fuzzy_result$centers #cluster centers

# ------------------------------3. Membership Degree Matrix and centering------------------------------
fuzzy_membership_matrix <- fuzzy_result$membership    # Membership matrix (values range from 0 to 1)
initial_centers <- fuzzy_result$centers               # Initial cluster centers
final_centers <- t(fuzzy_result$centers)              # Transposed cluster centers (if needed)

# ------------------------------4. Interpret the Clustering Results: Convert to Hard Clusters------------------------------
# For each sample, choose the cluster with the highest membership as the hard label
cluster_data$cluster <- apply(fuzzy_result$membership, 1, which.max)


# Combine the membership matrix with the original data for a clearer view of each sample's clustering
cluster_membership <- as.data.frame(fuzzy_result$membership)
data_with_clusters <- cbind(cluster_data, cluster_membership)

# Check columns
colnames(data_with_clusters)
dim(data_with_clusters)

saveRDS(data_with_clusters,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/fuzzy_clusters.rds")


# ------------------------------5. Evaluation------------------------------
# 5.1 Fuzzy Partition Coefficient (FPC) 
fpc_value <- fuzzy_result$betweenss / fuzzy_result$withinerror
cat("Fuzzy partition coefficient (betweenss/withinerror):", round(fpc_value, 2), "\n")

# 5.2 Silhouette 
dist_mat <- dist(cluster_data[, -ncol(cluster_data)])
hard_labels <- cluster_data$cluster

stats <- cluster.stats(dist_mat, hard_labels)
avg_sil <- stats$avg.silwidth
cat("Average silhouette width:", round(avg_sil, 3), "\n")

saveRDS(avg_sil,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/fuzzy_silhoutte.rds")
saveRDS(fpc_value,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/fuzzy_fpc.rds")
