# Summary: removed neuroscore and rows with 5+ missing values
library(tidyverse)
rm(list = ls())
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_clean.rds')

# COLUMN ==================
## Missing per column  ==========================
# Compute proportion of missing values per column
missing_col <- colMeans(is.na(data))  # This calculates missing percentage per column

# Create dataframe for plotting
missing_col_df <- data.frame(Column = names(missing_col), ProportionMissing = missing_col)

## Plot ===================
# Compute 80th, 85th, and 90th percentile values
quantile_80 <- quantile(missing_col, 0.80)  # 80% threshold
quantile_85 <- quantile(missing_col, 0.85)  # 85% threshold
quantile_90 <- quantile(missing_col, 0.90)  # 90% threshold

# Plot histogram with vertical lines at 80%, 85%, and 90%
ggplot(missing_col_df, aes(x = ProportionMissing)) +
  geom_histogram(fill = "blue", alpha = 0.4, bins = 30) +  # Adjust bin count if needed
  geom_vline(xintercept = quantile_80, color = "red", linetype = "dashed", size = 1) +  # 80% line
  geom_vline(xintercept = quantile_85, color = "green", linetype = "dashed", size = 1) +  # 85% line
  geom_vline(xintercept = quantile_90, color = "purple", linetype = "dashed", size = 1) +  # 90% line
  labs(title = "Distribution of Missingness Per Column",
       x = "Proportion of Missing Values",
       y = "Count of Columns") +
  theme_minimal()

## Remove columns ================
# Do not remove any columns
# Removing columns with x missingness
# data_cleaned_col <- data[, colMeans(!is.na(data)) > 0.8]

# ROW  =========================
## Missing per row =====================
data_cleaned <- data
missing_row <- apply(data_cleaned, 1, function(row) mean(is.na(row)))
missing_row_df <- data.frame(Person = 1:nrow(data_cleaned), ProportionMissing = missing_row)

## Plotting missingness in rows ====================
# Calculate 80th and 85th percentile values
quantile_80 <- quantile(missing_row, 0.80)  # 80% threshold
quantile_85 <- quantile(missing_row, 0.85)  # 85% threshold
quantile_90 <- quantile(missing_row, 0.90)  # 85% threshold

# Plot histogram with vertical lines at 80% and 85%
ggplot(missing_row_df, aes(x = ProportionMissing)) +
  geom_histogram(fill = "blue", alpha = 0.4, bins = 30) +  # Adjust bin count if needed
  geom_vline(xintercept = quantile_80, color = "red", linetype = "dashed", size = 1) +  # 80% line
  geom_vline(xintercept = quantile_85, color = "green", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = quantile_90, color = "purple", linetype = "dashed", size = 1) + 
  labs(title = "Density of Missingness per Person",
       x = "Proportion of Missing Values",
       y = "Density") +
  theme_minimal() 

# ggplot(missing_df, aes(x = ProportionMissing)) +
#   geom_histogram(fill = "blue", alpha = 0.4) +
#   labs(title = "Density of Missingness per Person",
#        x = "Proportion of Missing Values",
#        y = "Density") +
#   theme_minimal()

## Remove =====================
# Removing rows with x missingness
data_cleaned_full <- data[which(rowMeans(!is.na(data)) > 0.8709677), ]
dim(data_cleaned_full)

## NEW missingness plot =================
# Plotting missingness in rows
missing_full <- apply(data_cleaned_full, 1, function(row) mean(is.na(row)))
missing_full_df <- data.frame(Person = 1:nrow(data_cleaned_full), ProportionMissing = missing_full)

# Calculate 80th and 85th percentile values
quantile_80 <- quantile(missing_full, 0.80)  # 80% threshold
quantile_85 <- quantile(missing_full, 0.85)  # 85% threshold
quantile_90 <- quantile(missing_full, 0.90)  # 85% threshold

# Plot histogram with vertical lines at 80% and 85%
ggplot(missing_full_df, aes(x = ProportionMissing)) +
  geom_histogram(fill = "blue", alpha = 0.4, bins = 30) +  # Adjust bin count if needed
  geom_vline(xintercept = quantile_80, color = "red", linetype = "dashed", size = 1) +  # 80% line
  geom_vline(xintercept = quantile_85, color = "green", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = quantile_90, color = "purple", linetype = "dashed", size = 1) + 
  labs(title = "Density of Missingness per Person",
       x = "Proportion of Missing Values",
       y = "Density") +
  theme_minimal() 

# Removed neuroscore and rows with 5+ missing values
saveRDS(data_cleaned_full, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

