# ------------------------------------------------------------------------------
# Purpose: This script assesses how disease outcomes vary across clusters using 
# logistic regression. It visualizes adjusted odds ratios via heatmaps and evaluates 
# model performance with ROC curves and AUC metrics
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(gplots)
library(fastDummies)
library(pROC)
library(ROCR)
ad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Alzheimer/Outputs/output_final.rds')
ckd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Chronic_Kidney_Disease/Outputs/output_final.rds')
cad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Coronary_Artery_Disease/Outputs/output_final.rds')
diabetes <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Diabetes/Outputs/output_final.rds')
pd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Parkinson/Outputs/output_final.rds')
ukb_final_reduced <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')
data_read <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds') 

sum(ad$case)
sum(ckd$case)
data <- data_read$data
classification <- data_read$classification
merged_data <- cbind(data, cluster = classification)
merged_data <- as.data.frame(merged_data)
merged_data$id <- rownames(ukb_final_reduced)

# Merging data and disease cases ===============================================
disease_data <- merged_data %>% left_join(ad %>% dplyr::select(eid, case), by = c("id" = "eid")) %>%
  rename(AD = case)

disease_data <- disease_data %>% left_join(ckd %>% dplyr::select(eid, case), by = c("id" = "eid")) %>%
  rename(CKD = case)

disease_data <- disease_data %>% left_join(cad %>% dplyr::select(eid, case), by = c("id" = "eid")) %>%
  rename(CAD = case)

disease_data <- disease_data %>% left_join(diabetes %>% dplyr::select(eid, case), by = c("id" = "eid")) %>%
  rename(Diabetes = case)

disease_data <- disease_data %>% left_join(pd %>% dplyr::select(eid, case), by = c("id" = "eid")) %>%
  rename(PD = case)

# Number of diseases in the dataset ============================================
sum(disease_data$AD)
sum(disease_data$CKD)
sum(disease_data$CAD)
sum(disease_data$Diabetes)
sum(disease_data$PD)

# Number of diseases each cluster ==============================================
df_cluster_population <- disease_data %>%
  group_by(cluster) %>%
  summarise(Total_Population = n())  # Count number of rows per cluster

# Number of controls in each cluster ============================================
df_controls <- disease_data %>%
  filter(AD == 0 & CKD == 0 & CAD == 0 & Diabetes == 0 & PD == 0) %>%  # Keep only rows where all diseases = 0
  group_by(cluster) %>%
  summarise(Controls = n())  # Count number of rows (controls) in each cluster

# Group by cluster =============================================================
disease_cleaned <- disease_data %>% group_by(cluster) %>% summarise(across(c(AD, CKD, CAD, Diabetes, PD), sum))

disease_counts_cluster <- disease_cleaned %>% 
  left_join(df_controls, by ='cluster') 

disease_counts_cluster <- disease_counts_cluster %>%
  mutate(total_cases = rowSums(across(c(AD, CKD, CAD, Diabetes, PD))))

disease_counts_cluster <- disease_counts_cluster %>% 
  left_join(df_cluster_population, by = 'cluster')

disease_counts_cluster <- disease_counts_cluster %>%
  mutate(cluster = as.character(cluster)) %>%  # Convert cluster to character
  bind_rows(summarise(., cluster = "Total", across(c(AD, CKD, CAD, Diabetes, PD, total_cases, Controls, Total_Population), sum)))

disease_counts_cluster <- disease_counts_cluster %>%
  mutate(Control_Percentage = (Controls / Total_Population) * 100)

disease_counts_cluster <- disease_counts_cluster %>%
  relocate(Control_Percentage, .after = Controls)

disease_cleaned1 <- disease_cleaned[, -1]

# Correlation matrix ===========================================================
cor_matrix <- cor(disease_counts_cluster[, -1])  # Exclude the 'cluster' column
print(cor_matrix)

# Disease Prevalence Heatmap ===================================================

# Normalize disease counts by Total Population
df_normalized <- disease_counts_cluster %>%
  mutate(across(c(AD, CKD, CAD, Diabetes, PD), ~ . / Total_Population)) 
  #group_by(cluster) %>%
  #summarise(across(everything(), mean))  # Compute mean proportions per cluster

df_normalized <- df_normalized[1:7, 1:6]

# Compute mean disease proportions per cluster
df_numeric <- df_normalized %>%
  group_by(cluster) #%>%
  #summarise(across(everything(), mean))  # Keep only numeric data

# Convert to numeric matrix (excluding the 'cluster' column)
df_matrix <- as.matrix(df_numeric[, -1])  # Remove 'cluster' column
df_matrix <- t(df_matrix)

# Assign proper row and column names
rownames(df_matrix) <- colnames(df_numeric[, -1])  # Diseases as row labels
colnames(df_matrix) <- as.character(df_numeric$cluster)  # Cluster numbers as column labels

# Generate heatmap
heatmap(df_matrix, Colv = NA, Rowv = NA, scale = "row",
        col = brewer.pal(9, "YlOrRd"), margins = c(5,10))

# Define the YlOrBr color palette (Yellow-Orange-Brown)
heat_colors <- colorRampPalette(brewer.pal(9, "YlOrBr"))(100)
dev.new(width=15, height = 8)

# Order columns (clusters) numerically
df_matrix <- df_matrix[, order(as.numeric(colnames(df_matrix)))]
par(mar = c(2,2,2,2))
df_matrix_scaled <- t(apply(df_matrix, 1, function(x) (x - mean(x)) / sd(x)))

# Generate heatmap with axis labels and color bar
heatmap.2(df_matrix_scaled, 
          scale = "none",              # Normalize per row
          col = heat_colors,           # Use YlOrBr color scheme
          trace = "none",              # Remove unnecessary trace lines
          density.info = "none",       # No density plot
          margins = c(5, 10),          # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",              # Remove extra key title
          key.xlab = "Standardised Prevalence",        # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), # Adjust position of key
          labRow = rownames(df_matrix), # Keep disease names as row labels
          labCol = colnames(df_matrix),
          Rowv = FALSE,
          Colv = FALSE,
          symbreaks = TRUE,
          symkey = FALSE,
          cellnote = round(df_matrix_scaled, 2), # Add rounded OR values inside the heatmap
          notecol = "grey36",            # Text color for cell values
          notecex = 0.8)

# Logistic Regression ==========================================================
# One hot encode dataset
disease_data_encoded <- dummy_cols(disease_data, select_columns = 'cluster', remove_first_dummy = FALSE, remove_selected_columns = TRUE)

# Define the diseases and predictor
diseases <- c("AD", "CKD", "CAD", "Diabetes", "PD")
clusters <- c('cluster_1', 'cluster_2', 'cluster_3', 'cluster_4', 'cluster_5', 'cluster_6', 'cluster_7')

# Selection, train, and test ===================================================
set.seed(1342)
train <- sample(1:nrow(disease_data_encoded), floor(0.8 * nrow(disease_data_encoded)))
test <- seq(1, nrow(disease_data_encoded))[-train]
train_data <- disease_data_encoded[train, ]
test_data  <- disease_data_encoded[-train, ]

# Initialize a named vector to store AUC values
AUC_results <- matrix(NA, nrow = length(diseases), ncol = length(clusters), dimnames = list(diseases, clusters))

# Loop through each disease and compute AUC
for (disease in diseases) {
  for (cluster in clusters){
  
    # Create logistic regression model
    formula <- as.formula(paste0(disease, " ~ ", cluster))
    model <- glm(formula, family = binomial(link = "logit"), data = train_data)
    
    # Get predicted probabilities
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Compute AUC
    roc_obj <- roc(test_data[[disease]], predictions)
    AUC_results[disease, cluster] <- auc(roc_obj)
}}

# Print AUC values
print(AUC_results)

# AUC results heatmap ==========================================================
heat_colors <- colorRampPalette(c("blue", "white", "red"))(100)
min_val <- min(AUC_results, na.rm = TRUE)
max_val <- max(AUC_results, na.rm = TRUE)

# Create a breaks vector that forces 0.5 to be in the middle of the scale.
# This only works properly if your data spans below and above 0.5.
if(min_val < 0.5 & max_val > 0.5){
  # Create two sequences: one from min_val to 0.5, and one from 0.5 to max_val.
  # We use 51 values in each so that, after removing the duplicate center value, we have 101 breakpoints.
  breaks <- c(seq(min_val, 0.5, length.out = 51),
              seq(0.5, max_val, length.out = 51)[-1])
} else {
  # If your data do not span across 0.5, just use a regular sequence.
  breaks <- seq(min_val, max_val, length.out = 101)
}

heatmap.2(AUC_results,
          col = heat_colors,
          trace = "none",                   # Remove unnecessary trace lines
          density.info = "none",            # No density plot
          margins = c(5, 15),               # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",                   # Remove extra key title
          key.xlab = "AUC",                 # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = rownames(df_matrix),     # Keep disease names as row labels
          labCol = colnames(df_matrix),     # Adjust position of key
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(AUC_results, 3), # Add rounded OR values inside the heatmap
          notecol = "grey24",               # Text color for cell values
          notecex = 0.8,
          symbreaks = FALSE,
          symkey = FALSE,
          breaks = breaks)

# Odds ratio ===================================================================
diseases <- c("AD", "CKD", "CAD", "Diabetes", "PD")
clusters <- c('cluster_1', 'cluster_2', 'cluster_3', 'cluster_4', 'cluster_5', 'cluster_6', 'cluster_7')
or_results <- matrix(NA, nrow = length(diseases), ncol = length(clusters), dimnames = list(diseases, clusters))
pvalue_results <- matrix(NA, nrow = length(diseases), ncol = length(clusters), dimnames = list(diseases, clusters))

# Loop through each disease and compute AUC
for (disease in diseases) {
  for (cluster in clusters){
    
    # Create logistic regression model
    formula <- as.formula(paste0(disease, " ~ ", cluster))
    model <- glm(formula, family = binomial(link = "logit"), data = disease_data_encoded)
    
    coef_value <- coef(model)[cluster]
    p_value <- summary(model)$coefficients[-1, "Pr(>|z|)"]
    
    if (p_value < 0.05){
      pvalue_results[disease,cluster] <- p_value
    }
    
    or_results[disease,cluster] <- exp(coef_value)
    
  }}
    
df_or_results <- as.matrix(or_results)
df_pvalue_results <- as.matrix(pvalue_results)

print(df_or_results)
print(df_pvalue_results)
    
# OR Heatmap ===================================================================
heat_colors <- colorRampPalette(brewer.pal(9, "YlOrBr"))(100)
heat_colors <- colorRampPalette(c("blue", "white", "red"))(100)
min_val <- min(df_or_results, na.rm = TRUE)
max_val <- max(df_or_results, na.rm = TRUE)

# Create a vector of breakpoints that ensures 1 is at the midpoint
breaks <- c(seq(min_val, 1, length.out = 51), seq(1, max_val, length.out = 51)[-1])

heatmap.2(df_or_results, 
          #scale = "row",                     # Normalize per row
          col = heat_colors,                  # Use YlOrBr color scheme
          trace = "none", 
   # Remove unnecessary trace lines
          density.info = "none",              # No density plot
          margins = c(5, 15),                 # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",                     # Remove extra key title
          key.xlab = "Odds Ratio",            # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = rownames(df_matrix),       # Keep disease names as row labels
          labCol = colnames(df_matrix),       # Adjust position of key
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(df_or_results, 2), # Add rounded OR values inside the heatmap
          notecol = "antiquewhite4",          # Text color for cell values
          notecex = 1,
          symbreaks = TRUE,
          symkey = FALSE,
          breaks = breaks)

# OR Heatmap Sig Trace =========================================================
# Define diverging palette
heat_colors <- colorRampPalette(c("blue", "white", "red"))(100)

# Compute breaks ensuring 1 is at the midpoint (with 101 breakpoints for 100 colors)
min_val <- min(df_or_results, na.rm = TRUE)
max_val <- max(df_or_results, na.rm = TRUE)
breaks <- c(seq(min_val, 1, length.out = 51), seq(1, max_val, length.out = 51)[-1])

# Create heatmap with overlayed rectangles for significance
heatmap.2(df_or_results, 
          #scale = "row",                     # Normalize per row if needed (commented out here)
          col = heat_colors,                  # Use diverging color palette
          trace = "none", 
          density.info = "none",              # No density plot
          margins = c(5, 15),                 # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",                     # Remove extra key title
          key.xlab = "Odds Ratio",            # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = rownames(df_matrix),       # Disease names as row labels
          labCol = colnames(df_matrix),       # Cluster names as column labels
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(df_or_results, 2), # Display odds ratio values in cells
          notecol = "grey24",                 # Text color for cell values
          notecex = 0.8,
          symbreaks = TRUE,
          symkey = FALSE,
          breaks = breaks,
          add.expr = {
            # Get number of rows and columns in your odds ratio matrix
            nr <- nrow(df_or_results)
            nc <- ncol(df_or_results)
            # Loop over each cell
            for(i in 1:nr) {
              for(j in 1:nc) {
                # Check pvalue_results: if not NA, mark as significant
                if(!is.na(pvalue_results[i, j])) {
                  # Calculate y-coordinate (rows are flipped in the image)
                  y_coord <- nr - i + 1
                  # Draw a rectangle around the cell
                  rect(xleft = j - 0.5, 
                       ybottom = y_coord - 0.5, 
                       xright = j + 0.5, 
                       ytop = y_coord + 0.5, 
                       border = "black", 
                       lwd = 2)
                }
              }
            }
          }
)
