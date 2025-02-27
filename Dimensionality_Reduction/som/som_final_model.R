# Packages =====================================================================
library(factoextra)
library(NbClust)
library(kohonen)

# Read and format data =========================================================
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
data.sc <- as.matrix(ukb_cluster_encoded)

# Run SOM with optimized parameters ============================================
set.seed(0710001)
som.model <- som(data.sc, 
                 grid=somgrid(xdim=12, ydim=12, topo="hexagonal"), 
                 rlen=50000, alpha=c(0.05,0.01), keep.data =T)

saveRDS(som.model, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/som/som_model_12')