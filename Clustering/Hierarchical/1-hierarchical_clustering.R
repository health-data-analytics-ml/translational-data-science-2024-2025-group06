# ------------------------------------------------------------------------------
# Purpose: This script performs hierarchical clustering based on exposures
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(stats)
library(cluster)
library(fpc)
library(fastcluster)
set.seed(13874)
ukb_cluster <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_scaled.rds')
ukb_cluster <- ukb_cluster[1:25000,]

# Agglomerative and divisive cluster ===========================================
gower.dist = daisy(ukb_cluster, metric = "gower")
aggl.clust.c <- fastcluster::hclust(gower.dist, method = "complete")

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

data_aggl = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 8)))
data_divi = data.frame(t(cstats.table(gower.dist, divisive.clust, 10)))

# Save results =================================================================
saveRDS(gower.dist,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/gower_dist.rds")
saveRDS(data_aggl,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/data_aggl.rds")
saveRDS(data_divi,  "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/data_divi.rds")
