# Library ======================================================================
rm(list = ls())
library(UpSetR)
library(pheatmap)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pheatmap)
# Reading in pls_hat_parameters.rds and pls_selected_vars.rds - full lasso
selected_vars <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_selected_vars.rds')
hat_parameters <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_hat_parameters.rds')
log_output <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/log_output.rds")
table(log_output$cluster) # correct

# Save list of selected variables for each cluster =============================
protein_lists <- list()
for (i in 1:7) {
  cluster_name <- paste0('cluster_', i)
  threshold <- hat_parameters['pi', cluster_name]
  protein_lists[[cluster_name]] <- rownames(selected_vars)[selected_vars[, cluster_name] > threshold]
}
length(protein_lists$cluster_1) # 55 selected proteins
length(protein_lists$cluster_2) # 56 selected proteins
length(protein_lists$cluster_3) # 77 selected proteins
length(protein_lists$cluster_4) # 53 selected proteins
length(protein_lists$cluster_5) # 92 selected proteins
length(protein_lists$cluster_6) # 69 selected proteins
length(protein_lists$cluster_7) # 61 selected proteins

# Upset plot ===================================================================
## Convert selection proportions to 0, 1 (binary)
for (i in 1:7) {
  cluster_name <- paste0('cluster_', i)
  threshold <- hat_parameters['pi', cluster_name]
  selected_vars[, cluster_name] <- ifelse(selected_vars[, cluster_name] > threshold, 1, 0)
}
selected_df <- as.data.frame(selected_vars)
colnames(selected_df) <- gsub('cluster_', 'Cluster ', colnames(selected_df))
binary_df <- selected_df
binary_df[binary_df > 0] <- 1
save_dir <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/upset_plot_stability_lasso.png'
png(save_dir, width = 1600, height = 1200, res = 300)
upset(binary_df, 
      sets = rev(colnames(binary_df)), # Cluster 1 will print at the top
      keep.order = T,
      order.by = 'freq')
dev.off()

# Heatmap with odds ratios =====================================================
## Get data in correct format
heatmap_df <- log_output %>%
  select(Variable, cluster, odds_ratio) %>%
  pivot_wider(names_from = cluster, values_from = odds_ratio)
heatmap_matrix <- as.matrix(heatmap_df[, -1])
rownames(heatmap_matrix) <- heatmap_df$Variable
heatmap_matrix <- apply(heatmap_matrix, 2, as.numeric)
colnames(heatmap_matrix) <- gsub('cluster_', 'Cluster ', colnames(heatmap_matrix))

# Flatten the matrix and remove NA values
odds_ratios <- as.vector(heatmap_matrix)
odds_ratios <- odds_ratios[!is.na(odds_ratios)]

# Get quantiles to find min and max
quantile(odds_ratios, probs = c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1))

# Defining legend
log_or_matrix <- log(heatmap_matrix)
breaks <- seq(0, 2.11, length.out = 100)

## Plot
save_dir <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/heatmap.png'
png(save_dir, width = 1600, height = 1200, res = 300)
colnames(heatmap_matrix) <- 1:ncol(heatmap_matrix)
pheatmap(heatmap_matrix,
         color = colorRampPalette(c('blue', 'white', 'brown2'))(100),
         breaks = breaks,
         scale ='none',
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         na_col = 'grey60',
         main = 'Heatmap of Odds Ratio by Cluster',
         angle_col = 0)
dev.off()








