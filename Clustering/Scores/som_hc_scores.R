library(diceR)
library(cluster)

som.model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/som/som_model_12.rds')

som_codes <- som.model$codes[[1]]

dist_matrix <- dist(som_codes)  # Compute distance matrix
hclust_som <- hclust(dist_matrix, method = "ward.D2")  # Hierarchical clustering

# Determine number of clusters (e.g., k = 3)
num_clusters <- 5
clusters <- cutree(hclust_som, k = num_clusters)

# Compute PAC Score using diceR
pac_score <- PAC(hclust_som$height, lower = 2, upper = 18)

# Print PAC score
cat("PAC Score:", pac_score, "\n")


plot(hclust_som, labels = FALSE, main = "Hierarchical Clustering on SOM")
rect.hclust(hclust_som, k = num_clusters, border = "red")
