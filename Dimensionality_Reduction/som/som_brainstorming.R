rm(list = ls())

ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
toy <- ukb_cluster_encoded[sample(nrow(ukb_cluster_encoded), 1000, replace = FALSE), ]
View(toy)

## Load and prepare the gene expression data for analysis

# Genes in rows (gene symbols are row names), individuals in columns (dummy IDs used as column names)
### data <- read.delim("Test_dataset_GSE104954.txt")


## Filter out low-variance (likely non-informative) genes

#MAD - median absolute deviation - used as a measure of variance aross samples
### mad.genes <- apply(data, 1, mad) 

# filtered dataset after excluding genes with MAD<0.15 (threshold selected based on
#inspecting a histogram of MAD values distribution)
### data.f <- data[!c(row.names(data) %in% names(which(mad.genes<0.15))),] 

## Scale & center

#This is done to give the variables (genes) equal importance regardless of their
#expression levels (abundance)

### data.sc <- as.matrix(scale(t(data.f), center=T, scale=T))
data.sc <- data.sc <- as.matrix(ukb_cluster_encoded) # already scaled, no transposing needed

## Assess data clustering tendency

library(factoextra)
library(NbClust)

# res.dist <- get_dist(ukb_cluster_encoded, stand = TRUE, method = "spearman")
res.dist <- get_dist(toy, method = "spearman") # already scaled

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

get_clust_tendency(data.sc, n = 50,
                   gradient = list(low = "black",  high = "white"))



### SOM model 

library(kohonen)


## Optimize the map size

#Generate maps with different grid sizes from 4x4 through 12x12 (in some instances,
#asymmetrical maps may be preferred, e.g. 7x9)

set.seed(070801)
som4x4 <- som(data.sc, grid=somgrid(xdim=4, ydim=4, topo="hexagonal"))
set.seed(070802)
som5x5 <- som(data.sc, grid=somgrid(xdim=5, ydim=5, topo="hexagonal"))
set.seed(070803)
som6x6 <- som(data.sc, grid=somgrid(xdim=6, ydim=6, topo="hexagonal"))
set.seed(070804)
som7x7 <- som(data.sc, grid=somgrid(xdim=7, ydim=7, topo="hexagonal"))
set.seed(070805)
som8x8 <- som(data.sc, grid=somgrid(xdim=8, ydim=8, topo="hexagonal"))
set.seed(070806)
som9x9 <- som(data.sc, grid=somgrid(xdim=9, ydim=9, topo="hexagonal"))
set.seed(070807)
som10x10 <- som(data.sc, grid=somgrid(xdim=10, ydim=10, topo="hexagonal"))
set.seed(070808)
som11x11 <- som(data.sc, grid=somgrid(xdim=11, ydim=11, topo="hexagonal"))
set.seed(070809)
som12x12 <- som(data.sc, grid=somgrid(xdim=12, ydim=12, topo="hexagonal"))


#Plot map size vs distance as a mapping quality index; choose the simplest model (smallest size) that achieves 
#the best mapping (smallest distance)
mean.dist <- c(mean(som4x4$distances), mean(som5x5$distances), mean(som6x6$distances))
mean.dist <- c(mean(som4x4$distances), mean(som5x5$distances), mean(som6x6$distances),
               mean(som7x7$distances), mean(som8x8$distances), mean(som9x9$distances),
               mean(som10x10$distances),mean(som11x11$distances), mean(som12x12$distances))

plot(mean.dist, xaxt = "n", type="b")
axis(1, at=1:9, labels=c("4x4","5x5","6x6","7x7","8x8","9x9","10x10","11x11","12x12"))



## Run SOM with optimized parameters

set.seed(0710001)
som.model <- som(data.sc, 
                 grid=somgrid(xdim=6, ydim=6, topo="hexagonal"), 
                 rlen=50000, alpha=c(0.05,0.01), keep.data =T)


## Inspect the SOM model's diagnostic plots

plot(som.model, type="counts", shape="straight")
plot(som.model, type="mapping", shape="straight")
plot(som.model, type="changes") 
plot(som.model, type="dist.neighbours", palette.name=grey.colors, shape="straight")
plot(som.model, type="quality", shape="straight")


## Identify optimal number of clusters to cut

library(factoextra)
library(NbClust)

codes <- as.data.frame(som.model$codes)

fviz_nbclust(as.data.frame(som.model$codes), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")



## Cut clusters

hc <- cutree(hclust(dist(codes), method="ward.D2"), 4)


## Plot the SOM grid with clusters

plot(som.model, type="mapping", shape="straight", cex=1.5,
     bgcol=c("plum4","darkolivegreen4", "cornflowerblue","gold3")[hc], border="white")
add.cluster.boundaries(som.model, hc)