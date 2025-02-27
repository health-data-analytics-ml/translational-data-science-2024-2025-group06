# fastcluster
install.packages('fastcluster')
library('fastcluster')
hclust (d, method='complete', members=NULL)


# Hierarchical clustering
# all from https://medium.com/towards-data-science/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995

# Output from hierarchical_cluster.sh =============
agglom <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/aggl_clust_c.rds')
divisive <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/divisive_clust.rds')
gower.dist <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/gower_dist.rds')

# Setup and functions =================
library(fpc)
library(ggplot2)

# Author's function for comparing
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(4:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(5:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 4:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[5:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

# Agglomerative =========
## Dendogram ============
plot(agglom, main = "Agglomerative, complete linkages", labels = FALSE)

## Elbow ==============
ggplot(data = data.frame(t(cstats.table(gower.dist, agglom, 7))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Divisive =================
## Dendogram ============
plot(divisive, main = "Divisive", labels = FALSE)

## Elbow ==============
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive, 5))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

## Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive, 5))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# Take subset of data
ukb_cluster <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_scaled.rds')
cluster2 <- ukb_cluster[1:500,]

## Distance matrix ================
# Find dissimilarity matrix using gower
gower.dist = daisy(cluster2, metric = "gower")
class(gower.dist)

## Agglomerative ==================
# Agglomerative hierarchical clustering

aggl.clust.c <- hclust(gower.dist, method = "complete")

# Plot the dendogram
aggl.clust.c$order.lab <- rep("", 1000)
plot(aggl.clust.c, main = "Agglomerative, complete linkages", labels = FALSE)

data_aggl = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15)))

ggplot(data = data_aggl, 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

## Divisive ================
# Divisive hierarchical clustering, diss = TRUE as input is a distance matrix
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
divisive.clust$order.lab <- rep("", nval)
# Plots the dendogram
plot(divisive.clust, main = "Divisive", labRow = NULL)

## Comparison =================
# Author's function for comparing
library(fpc)
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
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 12)
stats.df.divisive

library(ggplot2)
# Elbow
# Divisive clustering
# the lower it is the closer the observations within the clusters
# are — changes for the different number of clusters. Ideally, 
# we should see a distinctive “bend”
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Agglomerative clustering,provides a more ambiguous picture
# Agglomerative “elbow” looks similar to that of divisive, except
# that agglomerative one looks smoother — with “bends” being less 
# abrupt.
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
# choose the number that maximizes the silhouette coefficient
# because you want clusters that are distinctive (far) enough to be 
# considered separate
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
# choose the number that maximizes the silhouette coefficient
# because you want clusters that are distinctive (far) enough to be 
# considered separate
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl_clust_c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))



# Follows An Introduction to Clustering in R p.47-
install.packages("factoextra")
install.packages("dendextend")
install.packages("NbClust")
install.packages("fpc")
## Packages =============
library(stats)
library(cluster)
library(factoextra) # clustering visualization
library(dendextend)
library(NbClust)

## Set-up ===========
nval <- 10000
toy <- ukb_cluster[1:nval,]
str(toy)
## Cluster ==========
# The single linkage method and the average linkage method could be run, 
# while Ward’s method should not be applied because the variables are not 
# quantitative
D <- daisy(toy, metric = "gower")
res.agnes <- agnes(D, diss = TRUE, # Must use distance matrix as input
                   method = "complete")
res.agnes$order.lab <- rep("", nval)
plot(res.agnes , hang = -0.1, which.plots = 2, main = "")

# From the dendogram you can see 5 clusters so specify this as k
k <- 11
cluster.agnes <- cutree(res.agnes , k = k)
table(cluster.agnes) # Number of people in each cluster

# Add the cluster (cluster.agnes) to the original dataframe
toy_with_cluster <- data.frame(toy, cluster.agnes)
# Select numeric columns and calculate mean per group
numeric_cols <- unlist(lapply(toy_with_cluster, is.numeric), use.names = FALSE)
df2 <- toy_with_cluster[numeric_cols] %>% group_by(cluster.agnes) %>% 
  summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()


par(mfrow = c(1,1))
for (j in 6:6){
  counts <- table(toy[, j], cluster.agnes)
  barplot(counts , main = names(toy)[j], 
          names.arg = paste("Clus.", colnames(counts)),
          xlab = round(chisq.test(counts)$p.value , 2),
          col = c("darkblue", "red", "green", "orange", "pink", "lightblue", "yellow"),
          legend = rownames(counts))
}
