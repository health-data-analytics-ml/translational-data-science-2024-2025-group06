# Load necessary libraries
library(ggplot2)

# Load data
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/bic_scores_3.RData')
load('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Clustering/Gaussian_Mixture_Model/silhouette_scores_3.RData')

# Define cluster range
clusters <- 4:20  

# Create dataframe
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

# Plot with improved scaling
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








