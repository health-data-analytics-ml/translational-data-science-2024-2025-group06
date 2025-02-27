rm(list = ls())
library(table1)
library(tableone)
library(ggplot2)

# Plotting numeric variables ===================================================
## List of numeric variables
ukb_final <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final.rds')
dev.off()
numeric_cols <- names(ukb_outliers_removed)[sapply(ukb_outliers_removed, function(x) is.numeric(x) | is.integer(x))]
length(numeric_cols)

## Plot the first nine numeric columns (fits best in graph)
first_nine <- numeric_cols[1:9]
par(mfrow = c(3, 3))
for (i in first_nine) {
  hist(ukb_outliers_removed[[i]], main = i, xlab = i)
}

## Plot numeric variables 10-14 (fits best in graph)
dev.off()
second_part <- numeric_cols[10:14]
par(mfrow = c(3, 2))
for (i in second_part) {
  hist(ukb_outliers_removed[[i]], main = i, xlab = i)
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

# 
# Before imputation ============================================================
ukb_outliers_removed <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_outliers_removed.rds')
all_cols <- colnames(ukb_outliers_removed)
no_demographics <- setdiff(all_cols, c('sex', 'ethnic_background', 'age'))
CreateTableOne(vars = no_demographics, data = ukb_outliers_removed)
# table1(~ no_demographics, data=ukb_outliers_removed)

# Post imputation, pre merge with proteins =====================================
ukb_imputed <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')
table1(~ ., data=ukb_imputed)

# Post merging with proteins ===================================================
table1(~ ., data=ukb_cluster)
