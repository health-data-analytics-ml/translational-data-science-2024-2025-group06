rm(list = ls())
library(ggplot2)
library(dplyr)
cluster_files <- paste0("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/univariate_cluster_", 1:7, ".rds")

# Manhattan plot ===============================================================
num_tests <- 1343
bonferroni_threshold <- 0.05 / num_tests # 3.723008e-05
sidak_threshold <- 1-(1-0.5) ^ (1/num_tests) # 0.0005159854

generate_manhattan_plot <- function(file_path, cluster_num) {
  cluster_data <- readRDS(file_path)
  cluster_data$logP <- -log10(cluster_data$p_value)
  cluster_data$significant <- cluster_data$logP > -log10(bonferroni_threshold)
  
  man_plot <- ggplot(cluster_data, aes(x = seq_along(protein), y = logP)) +
    geom_point(color = "royalblue4", alpha = 1, size = 1.7) +
    geom_hline(yintercept = -log10(bonferroni_threshold), linetype = "dashed", color = "red") + # need to change
    geom_text(data = cluster_data[cluster_data$significant, ], 
              aes(x = which(cluster_data$significant), y = logP, label = protein), 
              hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(color='red'),
          panel.border = element_blank(),
          panel.grid.major.x = element_line(colour = 'grey80'),
          panel.grid.major.y = element_line(colour = 'grey80'),
          panel.grid.minor.y = element_line(colour = 'grey80'),
          panel.background = element_rect(fill = 'grey99')) +
    labs(title = paste0("Cluster ", cluster_num, ": Manhattan Plot of Protein p-values"),
         caption = "Bonferroni") + 
    ylab(expression(-log[10](italic('p')))) + 
    xlab("Protein Index") +
    ylim(0, NA)
  save_path <- file.path(save_dir, paste0('manhattan_cluster_', cluster_num, '.png'))
  ggsave(save_path, plot = man_plot, width = 9, height = 6, dpi = 300)
}

save_dir <- "/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/manhattan_plots/"
for (i in 1:7) {
  generate_manhattan_plot(cluster_files[i], i)
}

# One plot for testing =========================================================
cluster_data <- readRDS(cluster_files[1])
cluster_data$logP <- -log10(cluster_data$p_value)
cluster_data$significant <- cluster_data$logP > -log10(bonferroni_threshold)

ggplot(cluster_data, aes(x = seq_along(protein), y = logP)) +
  geom_point(color = "royalblue4", alpha = 1, size = 1.7) +
  geom_hline(yintercept = -log10(bonferroni_threshold), linetype = "dashed", color = "red") + # need to change
  geom_text(data = cluster_data[cluster_data$significant, ], 
            aes(x = which(cluster_data$significant), y = logP, label = protein), 
            hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color='red'),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey80'),
        panel.grid.major.y = element_line(colour = 'grey80'),
        panel.grid.minor.y = element_line(colour = 'grey80'),
        panel.background = element_rect(fill = 'grey99')) +
  labs(title = paste0("Cluster ", 1, ": Manhattan Plot of Protein p-values"),
       caption = "Bonferroni") + 
  ylab(expression(-log[10](italic('p')))) + 
  xlab("Protein Index") +
  ylim(0, NA)

# save_path <- file.path(save_dir, paste0('manhattan_cluster_', 1, '.png'))
ggsave(save_path, plot, width = 10, height = 6, dpi = 300)


