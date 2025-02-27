rm(list = ls())

# Save each SOM model
## Define the output directory
output_dir <- "/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Dimensionality_Reduction/som/outputs/"

## Ensure the directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

som4x4 <- readRDS(file.path(output_dir, "som4x4.rds"))
som5x5 <- readRDS(file.path(output_dir, "som5x5.rds"))
som6x6 <- readRDS(file.path(output_dir, "som6x6.rds"))
som7x7 <- readRDS(file.path(output_dir, "som7x7.rds"))
som8x8 <- readRDS(file.path(output_dir, "som8x8.rds"))
som9x9 <- readRDS(file.path(output_dir, "som9x9.rds"))
som10x10 <- readRDS(file.path(output_dir, "som10x10.rds"))
som11x11 <- readRDS(file.path(output_dir, "som11x11.rds"))
som12x12 <- readRDS(file.path(output_dir, "som12x12.rds"))

# Plot map size vs distance as a mapping quality index; choose the simplest model (smallest size) that achieves 
# the best mapping (smallest distance)
mean.dist <- c(mean(som4x4$distances), mean(som5x5$distances), mean(som6x6$distances),
               mean(som7x7$distances), mean(som8x8$distances), mean(som9x9$distances),
               mean(som10x10$distances),mean(som11x11$distances), mean(som12x12$distances))
plot(mean.dist, xaxt = "n", type="b")
axis(1, at=1:9, labels=c("4x4","5x5","6x6","7x7","8x8","9x9","10x10","11x11","12x12"))
