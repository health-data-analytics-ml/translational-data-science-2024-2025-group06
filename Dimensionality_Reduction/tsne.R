rm(list = ls())
library(Rtsne)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_cluster_encoded <- data.frame(cluster = gmm_model$classification, ukb_cluster_encoded)

# Run t-SNE ====================================================================
ukb_cluster_encoded <- as.matrix(ukb_cluster_encoded)
tsne_result <- Rtsne(ukb_cluster_encoded, dims = 2, perplexity = 30, theta = 0.5, verbose = TRUE, max_iter = 1000)
saveRDS(tsne_result, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/tsne.rds')

