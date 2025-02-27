# Packages
rm(list = ls())
library(factoextra)
library(NbClust)
library(kohonen)

# Read and format data
ukb_cluster_encoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
data.sc <- as.matrix(ukb_cluster_encoded)

# SOM model

## Optimize the map size
## Generate maps with different grid sizes from 4x4 through 12x12 (in some instances,
## Asymmetrical maps may be preferred, e.g. 7x9)
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

# Save each SOM model
## Define the output directory
output_dir <- "/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Dimensionality_Reduction/som/outputs/"

## Ensure the directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

## Save SOM objects
saveRDS(som4x4, file.path(output_dir, "som4x4.rds"))
saveRDS(som5x5, file.path(output_dir, "som5x5.rds"))
saveRDS(som6x6, file.path(output_dir, "som6x6.rds"))
saveRDS(som7x7, file.path(output_dir, "som7x7.rds"))
saveRDS(som8x8, file.path(output_dir, "som8x8.rds"))
saveRDS(som9x9, file.path(output_dir, "som9x9.rds"))
saveRDS(som10x10, file.path(output_dir, "som10x10.rds"))
saveRDS(som11x11, file.path(output_dir, "som11x11.rds"))
saveRDS(som12x12, file.path(output_dir, "som12x12.rds"))
