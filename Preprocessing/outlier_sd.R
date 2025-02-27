rm(list = ls())
library(dplyr)
ukb_proteins_small <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_proteins_small.rds')

# Get rid of rows with an outlier in at least one column (+- 3 sd) =============
numeric_cols <- names(ukb_proteins_small)[sapply(ukb_proteins_small, function(x) is.numeric(x) | is.integer(x))]
count_outliers <- function(df, column_name) {
  mean_val <- mean(df[[column_name]], na.rm = TRUE)
  sd_val <- sd(df[[column_name]], na.rm = TRUE)
  upper_bound <- mean_val + 3 * sd_val
  lower_bound <- mean_val - 3 * sd_val
  outliers <- df[[column_name]] < lower_bound | df[[column_name]] > upper_bound
  return(sum(outliers, na.rm=TRUE))
}
outlier_counts <- sapply(numeric_cols, function(col) count_outliers(ukb_proteins_small, col))

# Number of outliers per numeric column
outlier_counts
# Only numeric columns that have more than 0 outliers
outlier_counts[outlier_counts > 0]
# Percentage of outliers in each numeric column
proportions <- outlier_counts[outlier_counts > 0] / nrow(ukb_proteins_small)
sort(proportions, decreasing = TRUE)

# Filtering out outliers for all numeric columns
ukb_no_outliers <- ukb_proteins_small
initial_rows <-nrow(ukb_no_outliers)

for (i in numeric_cols) {
  before_rows <- nrow(ukb_no_outliers)
  
  mean_val <- mean(ukb_no_outliers[[i]], na.rm = TRUE)
  sd_val <- sd(ukb_no_outliers[[i]], na.rm = TRUE)
  upper_bound <- mean_val + 3 * sd_val
  lower_bound <- mean_val - 3 * sd_val
  ukb_no_outliers <- filter(ukb_no_outliers, is.na(.data[[i]]) | (.data[[i]] <= upper_bound & .data[[i]] >= lower_bound))
  
  after_rows <- nrow(ukb_no_outliers)
  rows_removed <- before_rows - after_rows
  cat('Column: ', i, '- Rows removed: ', rows_removed, '\n')
}

total_removed <- nrow(ukb_proteins_small) - nrow(ukb_no_outliers)
total_removed
total_removed / nrow(ukb_proteins_small)
View(ukb_no_outliers)

# Save as RDS file =============================================================
saveRDS(ukb_no_outliers, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final.rds')




