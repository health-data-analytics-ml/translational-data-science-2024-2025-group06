# HDDC on HPC
library("HDclassif")
library("ggfortify")

set.seed(4327)
ukb_cluster <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')

# HDDC
prms <- hddc(ukb_cluster, K=4:10, model = "ALL", itermax = 20000, eps = 0.01)
# Add clusters found to dataframe
cluster_class <- data.frame(ukb_cluster, prms$class)
cluster_class$prms.class <- as.factor(cluster_class$prms.class)

# save dataframe
saveRDS(cluster_class, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/hddc_clusters.rds')

prms_pca <- prcomp(cluster_class)

autoplot(prms_pca, data = cluster_class, colour = "prms.class") +
  scale_color_brewer(palette = 'Set1')
