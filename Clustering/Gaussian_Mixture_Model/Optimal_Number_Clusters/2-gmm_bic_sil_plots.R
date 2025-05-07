# ------------------------------------------------------------------------------
# Purpose: This script plots the BIC and silhouette score for different numbers
# of GMM clusters separately and overlapping to determine the optimal number of clusters
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(ggplot2)
setwd('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model')
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/Optimal_Number_Clusters/bic_scores.RData')
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/Optimal_Number_Clusters/silhouette_scores.RData')

# BIC scores plot ==============================================================
k_range <- 4:20
bic_table <- data.frame(Clusters = k_range, BIC_Score = bic_scores)
print(head(bic_table))  # Check table structure

# Plot the BIC scores
bic_plot <- ggplot(bic_table, aes(x = Clusters, y = BIC_Score)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "BIC Scores for Different Cluster Counts",
       x = "Number of Clusters",
       y = "BIC Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
bic_plot
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Scoring/bic.png', 
       plot = bic_plot, width = 8, height = 6, dpi = 300)

# Silhouette scores plot =======================================================
silhouette_df <- data.frame(Clusters = k_range, Silhouette_Score = silhouette_scores)
silhouette_plot <- ggplot(silhouette_df, aes(x = Clusters, y = Silhouette_Score)) +
  geom_line(color = "red", size = 1) +  # Line plot
  geom_point(color = "red", size = 3) +  # Dots for each score
  labs(title = "Silhouette Score vs. Number of Clusters",
       x = "Number of Clusters",
       y = "Silhouette Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

silhouette_plot

ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Scoring/silhouette.png', 
       plot = silhouette_plot, width = 8, height = 6, dpi = 300)

# BIC and silhouette overlapping plot ==========================================
clusters <- 4:20  
df <- data.frame(
  clusters = clusters,
  BIC_Score = bic_scores,
  Silhouette_Score = silhouette_scores
)

# Remove missing values
df <- df[complete.cases(df), ]

# Dynamically adjust Y-axis limits
bic_min <- min(df$BIC_Score, na.rm = TRUE) - 5000
bic_max <- max(df$BIC_Score, na.rm = TRUE) + 5000
sil_min <- min(df$Silhouette_Score, na.rm = TRUE) - 0.01
sil_max <- max(df$Silhouette_Score, na.rm = TRUE) + 0.01

# Dynamically adjust scaling factor
scale_factor <- (bic_max - bic_min) / (sil_max - sil_min)

# Plot
p <- ggplot(df, aes(x = clusters)) +
  geom_line(aes(y = BIC_Score, color = "BIC Score"), size = 1) +
  geom_point(aes(y = BIC_Score, color = "BIC Score"), size = 2) +
  geom_line(aes(y = (Silhouette_Score - sil_min) * scale_factor + bic_min, color = "Silhouette Score"), 
            size = 1, linetype = "dashed") +
  geom_point(aes(y = (Silhouette_Score - sil_min) * scale_factor + bic_min, color = "Silhouette Score"), size = 2) +
  
  scale_y_continuous(
    name = "BIC Score",
    limits = c(bic_min, bic_max), 
    sec.axis = sec_axis(~ (. - bic_min) / scale_factor + sil_min, name = "Silhouette Score")
  ) +
  
  geom_vline(xintercept = 7, linetype = "dotted", color = "black", size = 1) +
  scale_color_manual(values = c("BIC Score" = "blue", "Silhouette Score" = "red")) +
  
  labs(x = "Number of Clusters", color = "Metrics", title = "BIC Score vs. Silhouette Score") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 30)
  )
p

ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Gaussian_Mixture_Model/Cluster_Scoring/bic_silhouette.png', plot = p, width = 12, height = 8, dpi = 300)
