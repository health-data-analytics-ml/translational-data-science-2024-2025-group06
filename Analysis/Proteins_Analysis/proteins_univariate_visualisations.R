rm(list = ls())
library(ggplot2)
library(dplyr)
cluster_files <- paste0("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/univariate_cluster_", 1:7, ".rds")

# Manhattan plot
num_tests <- 1343
bonferroni_threshold <- 0.05 / num_tests # 3.723008e-05
sidak_threshold <- 1-(1-0.5) ^ (1/num_tests) # 0.0005159854

generate_manhattan_plot <- function(file_path, cluster_num) {
  cluster_data <- readRDS(file_path)
  cluster_data$logP <- -log10(cluster_data$p_value)
  cluster_data$significant <- cluster_data$logP > -log10(sidak_threshold)
  man_plot <- ggplot(cluster_data, aes(x = seq_along(protein), y = logP)) +
    geom_point(aes(color = logP), alpha = 0.7, size = 2) +
    scale_color_gradient(low = "blue", high = "red") +
    geom_hline(yintercept = -log10(sidak_threshold), linetype = "dashed", color = "black") + # need to change
    geom_hline(yintercept = -log10(bonferroni_threshold), linetype = "dashed", color = "red") + # need to change
    geom_text(data = cluster_data[cluster_data$significant, ], 
              aes(x = which(cluster_data$significant), y = logP, label = protein), 
              hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
    labs(title = paste("Manhattan Plot of Protein p-values (Cluster", cluster_num, ")", sep = ''),
         x = "Protein Index",
         y = "-log10(p-value)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  save_path <- file.path(save_dir, paste0('manhattan_cluster_', cluster_num, '.png'))
  ggsave(save_path, plot = man_plot, width = 10, height = 6, dpi = 300)
}

save_dir <- "/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/"
for (i in 1:7) {
  generate_manhattan_plot(cluster_files[i], i)
}



# One plot for testing
cluster_data <- readRDS(cluster_files[1])
cluster_data$logP <- -log10(cluster_data$p_value)
cluster_data$significant <- cluster_data$logP > -log10(sidak_threshold)

ggplot(cluster_data, aes(x = seq_along(protein), y = logP)) +
  geom_point(aes(color = logP), alpha = 0.7, size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_hline(yintercept = -log10(sidak_threshold), linetype = "dashed", color = "black") + # need to change
  geom_hline(yintercept = -log10(bonferroni_threshold), linetype = "dashed", color = "red") + # need to change
  geom_text(data = cluster_data[cluster_data$significant, ], 
            aes(x = which(cluster_data$significant), y = logP, label = protein), 
            hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  labs(title = paste("Manhattan Plot of Protein p-values", cluster_num),
       x = "Protein Index",
       y = "-log10(p-value)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
save_path <- file.path(save_dir, paste0('manhattan_cluster_', cluster_num, '.png'))
ggsave(save_path, plot, width = 10, height = 6, dpi = 300)


