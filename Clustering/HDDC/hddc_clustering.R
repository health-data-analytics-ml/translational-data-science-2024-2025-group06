# hddc clustering
rm(list = ls())
# Setup ==========
#install.packages("HDclassif")
#install.packages("ggfortify")
library("HDclassif")

set.seed(4327)
ukb_cluster <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
cluster2 <- ukb_cluster[1:2000,]

# Finding the best clusters
prms <- hddc(cluster2, K=4:10, model = "ALL", itermax = 500, eps = 0.001, algo = "CEM")
prms
# Add cluster classification to original dataset
cluster2_class <- data.frame(cluster2, prms$class)
cluster2_class$prms.class <- as.factor(cluster2_class$prms.class)

saveRDS(cluster2_class, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/hddc_clusters.rds')


