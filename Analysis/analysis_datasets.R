# ------------------------------------------------------------------------------
# Purpose: This script creates the datasets necessary for the exposome and
# protein analysis
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(fastDummies)
library(qqman)
library(tidyverse)
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")


# ukb_analysis: dataset for univariate analysis (exposures) ====================
set.seed(1342)
scale <- sample(1:nrow(ukb_final_reduced), floor(0.8 * nrow(ukb_final_reduced)))
test <- seq(1, nrow(ukb_final_reduced))[-scale] # gets the 20% test set, 8361
select <- sample(scale, 0.625*length(scale)) # gets the 50% variable selection set, 20901
train <- setdiff(scale, select) # gets the 30% training set, 12542

# Merge the cluster classifications with the full dataset, categorise identified ethnic background
ukb_analysis <- data.frame(cluster = gmm_model$classification, ukb_final_reduced)
ukb_analysis <- ukb_analysis %>% mutate(ethnic_background = ifelse((ethnic_background == "British" | 
                                                                      ethnic_background == "Black or Black British" |
                                                                      ethnic_background == "Asian or Asian British"), "British_ethnic_backgrounds", "Other_identified_ethnic_backgrounds"))
ukb_analysis$ethnic_background <- as.factor(ukb_analysis$ethnic_background)

# Scale the training and test rows using just the training data
numeric_cols <- c("age", "neuro_score", "pack_years", "no2_2010", "pm10", "pm2.5",
                  "traffic_intensity", "inv_dis_maj_road", "greenspace_1000m",
                  "water_1000m", "coast_distance", 
                  "multiple_deprivation_index", "sleep_data", "diet_score", "met_score")
for(i in numeric_cols){
  mean <- mean(ukb_analysis[scale,i])
  sd <- sd(ukb_analysis[scale,i])
  ukb_analysis[,i] <- (ukb_analysis[,i] - mean)/ sd
}

# Create identity columns for each cluster
ukb_analysis$ids <- row.names(ukb_analysis)
ukb_analysis <- dummy_cols(ukb_analysis, select_columns = 'cluster')

# Add the rownames back in, this is removed by dummy_cols
rownames(ukb_analysis) <- ukb_analysis$ids
ukb_analysis <- ukb_analysis[,-36]

saveRDS(ukb_analysis, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")


# ukb_analysis_LASSO: dataset for stability selection LASSO (exposures) ========
rm(list = ls())
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")

# remove cluster columns
ukb_analysis <- ukb_analysis[,-c(36:42)]
ukb_analysis$sex <- as.numeric(ukb_analysis$sex)
ukb_analysis$ethnic_background <- as.numeric(ukb_analysis$ethnic_background)

# One-hot encode factor columns
factor_cols <- names(ukb_analysis)[sapply(ukb_analysis, is.factor)]
ukb_analysis$ids <- row.names(ukb_analysis)
ukb_analysis <- dummy_cols(ukb_analysis, 
                          select_columns = factor_cols,
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)

ukb_analysis <- dummy_cols(ukb_analysis, select_columns = 'cluster')
rownames(ukb_analysis) <- ukb_analysis$ids
ukb_analysis <- ukb_analysis[,-19]

saveRDS(ukb_analysis, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis_LASSO.rds")


# ukb_proteins_univariate: dataset for univariate analysis (proteins) ==========
## Full dataframe with proteins, without outliers removed
ukb_proteins_merge <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_proteins_merge.rds')

## Only include the rows from ukb_final (the rows that remain once the outliers are removed)
ukb_final <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final.rds')
common_rows <- intersect(rownames(ukb_proteins_merge), rownames(ukb_final))
ukb_proteins_univariate <- ukb_proteins_merge[common_rows,]

## Remove all the exposure data besides sex, age, and ethnicity, scale age
ukb_proteins_univariate <- ukb_proteins_univariate[, -c(2:16, 19:34)]
ukb_proteins_univariate$age <- scale(ukb_proteins_univariate$age)

## Merge the cluster classifications with the full dataset, categorise identified ethnic background
ukb_proteins_univariate <- data.frame(cluster = gmm_model$classification, ukb_proteins_univariate)
ukb_proteins_univariate <- ukb_proteins_univariate %>% mutate(ethnic_background = ifelse((ethnic_background == "British" | 
                                                                      ethnic_background == "Black or Black British" |
                                                                      ethnic_background == "Asian or Asian British"), "British_ethnic_backgrounds", "Other_identified_ethnic_backgrounds"))
ukb_proteins_univariate$ethnic_background <- as.factor(ukb_proteins_univariate$ethnic_background)

## Create identity columns for each cluster, drop original cluster column
ukb_proteins_univariate <- dummy_cols(ukb_proteins_univariate, select_columns = 'cluster')
ukb_proteins_univariate$cluster <- NULL
saveRDS(ukb_proteins_univariate, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_univariate.rds")


# ukb_proteins_lasso.rds: dataset for stability selection LASSO (proteins) =====
ukb_proteins_univariate$sex <- as.numeric(ukb_proteins_univariate$sex)
ukb_proteins_univariate$ethnic_background <- as.numeric(ukb_proteins_univariate$ethnic_background)
saveRDS(ukb_proteins_univariate, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_lasso.rds")


# Saving cluster names =========================================================
cluster_names <- data.frame(Cluster = c("cluster_1", "cluster_2","cluster_3",
                                        "cluster_4","cluster_5","cluster_6",
                                        "cluster_7"),
                            cluster_name = c("Cluster 1", "Cluster 2", "Cluster 3",
                                             "Cluster 4", "Cluster 5", "Cluster 6",
                                             "Cluster 7"))
saveRDS(cluster_names, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/cluster_names.rds")

