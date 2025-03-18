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
save_dir <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/upset_plot.png'
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

## Plot
save_dir <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/heatmap.png'
png(save_dir, width = 1600, height = 1200, res = 300)
pheatmap(heatmap_matrix,
         color = colorRampPalette(c('blue', 'grey95', 'red'))(100),
         scale ='none',
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         na_col = 'grey80',
         # display_numbers = TRUE,
         main = 'Heatmap of Odds Ratio by Cluster')
dev.off()















library(ggplot2)
library(reshape2)

# Convert matrix to long format
heatmap_data <- melt(heatmap_matrix)

# Rename columns for clarity
colnames(heatmap_data) <- c("Feature", "Cluster", "Odds_Ratio")
ggplot(heatmap_data, aes(x = Cluster, y = Feature, fill = Odds_Ratio)) +
  geom_tile(color = NA) +  # No borders around tiles
  scale_fill_gradientn(colors = c("white", "deepskyblue", "red")) +  # Color scale like pheatmap
  theme_minimal() +  # Clean look
  labs(title = "Heatmap of Odds Ratio by Cluster",
       x = NULL,  # No x-axis label (like pheatmap)
       y = "Features") +  # Y-axis label
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Title centered & bold
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate cluster names
    panel.background = element_rect(fill = "grey95", color = NA),  # Light gray background
    plot.background = element_rect(fill = "grey95", color = NA),  # Full background gray
    panel.grid = element_blank()  # Remove grid lines
  )


categorize_values <- function(x) {
  if (is.na(x)) {
    return(NA)  # Keep NA values
  } else if (x == 0) {
    return("grey90")  # Light gray for 0
  } else if (x <= 0.5) {
    return("deepskyblue")  # Blue for (0, 0.5]
  } else if (x <= 1) {
    return("blue")  # Darker blue for (0.5, 1]
  } else {
    return("red")  # Red for >1
  }
}# Convert matrix values into their corresponding colors
color_matrix <- matrix(sapply(heatmap_matrix, categorize_values), 
                       nrow = nrow(heatmap_matrix), 
                       ncol = ncol(heatmap_matrix),
                       dimnames = dimnames(heatmap_matrix))
pheatmap(heatmap_matrix,
         color = c("grey90", "deepskyblue", "blue", "red"),  # Defined categories
         breaks = c(-Inf, 0, 0.5, 1, Inf),  # Define cutoff points
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         na_col = "white",  # Keep NA values as white
         main = 'Heatmap of Odds Ratio by Cluster')

