# Load libraries, read in data =================================================
rm(list = ls())
library(mclust)
library(dplyr)
library(Rtsne)
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# Creating GMM =================================================================
## Test out 4-20 clusters, look for what number of clusters has the lowest BIC
set.seed(123)
bic_scores <- numeric(length = 20-4+1)
names(bic_scores) <- 4:20
for (i in 4:20) {
  gmm_model <- Mclust(ukb_cluster_encoded, G=i)
  bic_scores[as.character(i)] <- gmm_model$BIC
}
saveRDS(bic_scores, '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores.rds')
bic_scores
## Unfortunately BIC decreases with increasing number of clusters, chose 10

## Run final model with specified number of clusters ===========================
set.seed(43)
gmm_model <- Mclust(ukb_cluster_encoded, G=10)
cluster_assignments <- gmm_model$classification
saveRDS(gmm_model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
       