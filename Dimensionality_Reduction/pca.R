rm(list = ls())
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# Run PCA
pca_result <- prcomp(ukb_cluster_encoded, center = TRUE, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x)
saveRDS(pca_result, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_result.rds')
saveRDS(pca_scores, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/pca_scores.rds')
