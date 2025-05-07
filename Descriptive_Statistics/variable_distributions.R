# ------------------------------------------------------------------------------
# Purpose: This script plots distributions of numeric, categorical, and binary
# exposure variables
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(table1)
library(tableone)
library(ggplot2)
ukb_final <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')

# Plotting numeric variables ===================================================
## List of numeric variables
dev.off()
numeric_cols <- names(ukb_final)[sapply(ukb_final, function(x) is.numeric(x) | is.integer(x))]
length(numeric_cols)

## Plot the first nine numeric columns (fits best in graph)
first_nine <- numeric_cols[1:9]
par(mfrow = c(3, 3))
for (i in first_nine) {
  hist(ukb_final[[i]], main = i, xlab = i)
}

## Plot numeric variables 10-14 (fits best in graph)
dev.off()
second_part <- numeric_cols[10:14]
par(mfrow = c(3, 2))
for (i in second_part) {
  hist(ukb_final[[i]], main = i, xlab = i)
}

# Plotting binary variables ====================================================
dev.off()
binary <- c('gas_cooker', 'gas_fire', 'solid_fire')
par(mfrow = c(1, 3))
for (i in binary) {
  counts <- table(ukb_final[[i]])
  barplot(counts, main = paste(i, ' Distribution'), xlab = i)
}

# Plotting categorical variables ===============================================
## List of categorical variables
cat_columns <- names(ukb_final)[sapply(ukb_final, is.factor)]
cat_columns <- setdiff(cat_columns, binary)
length(cat_columns)

## alcohol_intake -> Alcohol Intake
format_col_name <- function(col_name) {
  formatted_name <- gsub('_', ' ', col_name)
  formatted_name <- tools::toTitleCase(formatted_name)
  return(formatted_name)
}

## Plot
dev.off()
cat_columns
for (i in cat_columns) {
  formatted_label <- format_col_name(i)
  p <- ggplot(ukb_final, aes(x = .data[[i]])) +
    geom_bar() +
    coord_flip() +
    theme_minimal() +
    ggtitle(paste(formatted_label, 'Distribution')) +
    ylab('Count') +
    xlab(formatted_label)
  print(p)
}
