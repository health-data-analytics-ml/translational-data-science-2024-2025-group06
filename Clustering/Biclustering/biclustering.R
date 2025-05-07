# ------------------------------------------------------------------------------
# Purpose: This script attempts biclustering of our data based on exposures and 
# prints resulting heatmaps
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(pheatmap)
library(biclust)
library(tidyverse)
setwd('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Biclustering')
data <- as.matrix(readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds'))
toy_data <- data[1:1000,]

# Run biclustering =============================================================
set.seed(8)
cluster <- biclust(data, method = BCPlaid())
summary(cluster)

# Print heatmaps ===============================================================
par(mar = c(10, 10, 10, 10))
pdf('pheatmap.pdf', height = 50, width = 6)
pheatmap(data, cluster_rows = TRUE, cluster_cols = TRUE)
dev.off()

par(mar = c(10, 10, 1, 10))
pdf('heatmap.pdf', height = 50, width = 6)

heatmapBC(bicResult = cluster, x = data, fontsize_col = 1)
dev.off()







  
