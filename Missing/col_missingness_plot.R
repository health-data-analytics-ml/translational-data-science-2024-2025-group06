library(ggplot2)
library(tidyverse)

# Load dataset
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')
data <-  ukb_clean

# Compute proportion of missing values per column
missing <- colMeans(is.na(data))  # This calculates missing percentage per column

# Create dataframe for plotting
missing_df <- data.frame(Column = names(missing), ProportionMissing = missing)

# Compute 80th, 85th, and 90th percentile values
quantile_80 <- quantile(missing, 0.80)  # 80% threshold
quantile_85 <- quantile(missing, 0.85)  # 85% threshold
quantile_90 <- quantile(missing, 0.90)  # 90% threshold

# Plot histogram with vertical lines at 80%, 85%, and 90%
ggplot(missing_df, aes(x = ProportionMissing)) +
  geom_histogram(fill = "blue", alpha = 0.4, bins = 30) +  # Adjust bin count if needed
  geom_vline(xintercept = quantile_80, color = "red", linetype = "dashed", size = 1) +  # 80% line
  geom_vline(xintercept = quantile_85, color = "green", linetype = "dashed", size = 1) +  # 85% line
  geom_vline(xintercept = quantile_90, color = "purple", linetype = "dashed", size = 1) +  # 90% line
  labs(title = "Distribution of Missingness Per Column",
       x = "Proportion of Missing Values",
       y = "Count of Columns") +
  theme_minimal()
