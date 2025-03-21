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
save(bic_scores, file='/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores.RData')
# load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores.RData')

bic_table <- data.frame(Clusters = as.numeric(names(bic_scores)), BIC_Score = bic_scores)
View(bic_table)
# Plot the BIC scores
p <- ggplot(bic_table, aes(x = Clusters, y = BIC_Score)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "BIC Scores for Different Cluster Counts",
       x = "Number of Clusters",
       y = "BIC Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Save plot as PNG
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/gmm_bic.png', plot = p, width = 8, height = 6, dpi = 300)

## Chose 7 because that is right before the BIC jumps up, also looked at silhouette score

## Run final model with specified number of clusters ===========================
set.seed(1)
gmm_model <- Mclust(ukb_cluster_encoded, G=7)
summary(gmm_model)
cluster_assignments <- gmm_model$classification
saveRDS(gmm_model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')


       