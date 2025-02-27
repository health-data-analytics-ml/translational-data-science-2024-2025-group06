library(mclust)
library(diceR)
library(cluster)
library(factoextra)
set.seed(8)

data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')


# GAP Statistics for Hierarchical  ----------------------------------------

hc <- hclust(dist(data), method = "ward.D2")

# Compute gap statistic
gap_stat <- clusGap(data, FUN = function(x, k) hclust(dist(x), method = "ward.D2"), K.max = 10, B = 500)

# Print gap statistics
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)