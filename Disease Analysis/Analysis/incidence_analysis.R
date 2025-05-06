library(tidyverse)

ad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Alzheimer/Outputs/output_final.rds')
ckd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Chronic_Kidney_Disease/Outputs/output_final.rds')
cad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Coronary_Artery_Disease/Outputs/output_final.rds')
diabetes <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Diabetes/Outputs/output_final.rds')
pd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Parkinson/Outputs/output_final.rds')

ukb_final_reduced <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')
data_read <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds') 



# Merging ID and clusters  ------------------------------------------------
data <- data_read$data
classification <- data_read$classification

merged_data <- cbind(data, cluster = classification)
merged_data <- as.data.frame(merged_data)

merged_data$id <- rownames(ukb_final_reduced)



# Merging Diseases --------------------------------------------------------

ad_df <- merged_data %>% left_join(ad %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

ckd_df <- merged_data %>% left_join(ckd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

cad_df <- merged_data %>% left_join(cad %>% dplyr::select(eid, incident_case), by = c("id" = "eid"))

pd_df <- merged_data %>% left_join(pd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

diabetes_df <- merged_data %>% left_join(diabetes %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

disease_df_list <- list(
  ad_df = ad_df,
  ckd_df = ckd_df,
  cad_df = cad_df,
  pd_df = pd_df,
  diabetes_df = diabetes_df
)


# Removing NAs ------------------------------------------------------------
new_list <- list()
for (i in seq_along(disease_df_list)) {
  new_list[[i]] <- na.omit(disease_df_list[[i]])
}

ad_df <- new_list[[1]]
cad_df <- new_list[[2]]
ckd_df <- new_list[[3]]
diabetes_df <- new_list[[4]]
pd_df <- new_list[[5]]


disease_df_list <- list(
  ad_df = ad_df,
  ckd_df = ckd_df,
  cad_df = cad_df,
  pd_df = pd_df,
  diabetes_df = diabetes_df
)

# Creating Summary Table --------------------------------------------------


# Example: Create a named list of disease data frames
# (Make sure the list names match the disease names you want as column headers.)
disease_df_list <- list(
  AD = ad_df,
  CKD = ckd_df,
  CAD = cad_df,
  PD = pd_df,
  Diabetes = diabetes_df
)

# Loop through each data frame in the list, group by 'cluster' and sum 'incidence_case'
summary_list <- lapply(names(disease_df_list), function(disease_name) {
  df <- disease_df_list[[disease_name]]
  df_summary <- df %>%
    group_by(cluster) %>%
    summarise(total_incidence = sum(incident_case, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(disease = disease_name)
  return(df_summary)
})

# Combine all summaries into one long-format data frame
summary_long <- bind_rows(summary_list)

# Pivot the long-format data to wide format, with clusters as rows and diseases as columns
summary_wide <- summary_long %>%
  pivot_wider(names_from = disease, values_from = total_incidence, values_fill = list(total_incidence = 0))

# Creating total_incidence
summary_wide <- summary_wide %>%
  mutate(total_incidence = rowSums(dplyr :: select(., -cluster), na.rm = TRUE))

# Creating Total row
# Convert the 'cluster' column in summary_wide to character, if it's not already
summary_wide <- summary_wide %>% mutate(cluster = as.character(cluster))

# Compute totals for numeric columns and add a 'cluster' label for the total row
total_row <- summary_wide %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  mutate(cluster = "Total") %>% 
  dplyr::select(cluster, dplyr::everything())

# Append the total row to the original data frame
summary_wide <- bind_rows(summary_wide, total_row)

# View the updated summary_wide
print(summary_wide)




# Incidence Heatmap -------------------------------------------------------

# How would i do it?


# One Hot Encode ----------------------------------------------------------

library(fastDummies)
disease_df_list <- list(
  ad_df = ad_df,
  ckd_df = ckd_df,
  cad_df = cad_df,
  pd_df = pd_df,
  diabetes_df = diabetes_df
)

# Loop through the list and apply dummy_cols() to encode the 'cluster' column
for(i in seq_along(disease_df_list)) {
  disease_df_list[[i]] <- dummy_cols(disease_df_list[[i]], 
                                     select_columns = "cluster", 
                                     remove_first_dummy = FALSE, 
                                     remove_selected_columns = TRUE)
}

# Optionally, if you want to update the individual variables in your workspace:
list2env(disease_df_list, envir = .GlobalEnv)


# Incidence logistic regression per cluster -------------------------------

# Creating Empty Matrix
clusters <- c('cluster_1', 'cluster_2', 'cluster_3', 'cluster_4', 'cluster_5', 'cluster_6', 'cluster_7')

logistic_cluster_result <-  matrix(NA, nrow = 7, ncol = 6)

colnames(logistic_cluster_result) <- c('Cluster', "AD", "PD", "CKD", "CAD",  "Diabetes")

logistic_cluster_result[, "Cluster"] <- 1:7

p_value_result <- logistic_cluster_result



# Logistic Regression 

## Mapping vector: desired column names for the results matrix.
disease_names <- c("AD", "CKD", "CAD", "PD", "Diabetes")

# Loop over the list indices instead of names.
for(i in seq_along(disease_df_list)) {
  df <- disease_df_list[[i]]
  
  # For each cluster (from 1 to 7)
  for(j in 1:7) {
    # Construct the dummy variable name (should match what dummy_cols() produced)
    predictor_name <- paste0("cluster_", j)
    
    # Build the formula: incident_case ~ cluster_j
    formula_str <- paste("incident_case ~", predictor_name)
    
    # Fit the logistic regression model
    model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = df)
    
    # Extract the coefficient for the dummy variable (assumed to be the second coefficient)
    # and exponentiate it to obtain the odds ratio.
    odds_ratio <- exp(coef(model)[2])
    
    # Store the odds ratio in the matrix.
    # Rows correspond to cluster (j), and columns correspond to the disease.
    logistic_cluster_result[j, disease_names[i]] <- odds_ratio
    
    p_val <- summary(model)$coefficients[2, "Pr(>|z|)"]
    
    # If the p-value is less than 0.05, store it in the appropriate cell
    if(p_val < 0.05) {
      p_value_result[j, disease_names[i]] <- p_val
  }
  }
}

# View the resulting matrix


logistic_cluster_result <- t(logistic_cluster_result[, 2:ncol(logistic_cluster_result)])
print(logistic_cluster_result)


# extracting p-vales
p_value_result <- t(p_value_result[, 2:ncol(p_value_result)])
print(p_value_result)


# Odds Ratio Heatmap ------------------------------------------------------

# Define diverging palette
heat_colors <- colorRampPalette(c("dodgerblue", "white", "brown2"))(200)

# Compute breaks ensuring 1 is at the midpoint (with 101 breakpoints for 100 colors)
min_val <- min(logistic_cluster_result, na.rm = TRUE)
max_val <- max(logistic_cluster_result, na.rm = TRUE)
breaks <- c(seq(min_val, 1, length.out = 101), seq(1, max_val, length.out = 101)[-1])

# Create heatmap with overlayed rectangles for significance
heatmap.2(logistic_cluster_result, 
          #scale = "row",               # Normalize per row if needed (commented out here)
          col = heat_colors,           # Use diverging color palette
          trace = "none", 
          density.info = "none",       # No density plot
          margins = c(5, 15),          # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",              # Remove extra key title
          key.xlab = "Odds Ratio",     # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = c(expression(bold(AD) * " (450; 41,698)"),
                     expression(bold(PD) * " (1953; 39,518)"),
                     expression(bold(CKD) * " (1659; 40,429)"),
                     expression(bold(CAD) * " (11,129; 15,380)"),
                     expression(bold(Diabetes) * " (590; 41,646)")),      # Disease names as row labels
          labCol = c(expression(bold('1')),
                     expression(bold('2')),
                     expression(bold('3')),
                     expression(bold('4')),
                     expression(bold('5')),
                     expression(bold('6')),
                     expression(bold('7'))),
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(logistic_cluster_result, 2), # Display odds ratio values in cells
          notecol = "black",          # Text color for cell values
          notecex = 0.8,
          symbreaks = TRUE,
          symkey = FALSE,
          breaks = breaks,
          cexRow = 1.3,
          add.expr = {
            # Get number of rows and columns in your odds ratio matrix
            nr <- nrow(logistic_cluster_result)
            nc <- ncol(logistic_cluster_result)
            # Loop over each cell
            for(i in 1:nr) {
              for(j in 1:nc) {
                # Check pvalue_results: if not NA, mark as significant
                if(!is.na(p_value_result[i, j])) {
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




# AUC  --------------------------------------------------------------------

library(pROC)  # for roc() and auc()

disease_df_list <- list(
  AD = ad_df,
  PD = pd_df,
  CKD = ckd_df,
  CAD = cad_df,
  
  Diabetes = diabetes_df
)


# Define diseases and clusters vectors
diseases <- c("AD", "PD", "CKD", "CAD",  "Diabetes")
clusters <- c("cluster_1", "cluster_2", "cluster_3", "cluster_4", "cluster_5", "cluster_6", "cluster_7")

# Create an empty matrix to store AUC values.
AUC_results_1 <- matrix(NA, nrow = length(diseases), ncol = length(clusters),
                      dimnames = list(diseases, clusters))

# Loop over each disease data frame
for (disease in diseases) {
  # Get the data frame for this disease from your list
  df <- disease_df_list[[disease]]
  
  # (Optional) If you have a separate train/test split, do it here.
  # For this example, we'll use the full data frame as both train and test.
  set.seed(1342)
  train <- sample(1:nrow(df), floor(0.8 * nrow(df)))
  test <- seq(1, nrow(df))[-train]
  
  
  train_data <- df[train, ]
  test_data  <- df[-train, ]
  
  # Loop over each cluster dummy variable
  for (cluster in clusters) {
    # Build the logistic regression formula dynamically.
    # For example: "incident_case ~ cluster_1"
    formula_str <- paste("incident_case ~", cluster)
    
    # Fit the logistic regression model
    model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = train_data)
    
    # Get predicted probabilities on the test data
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Compute the ROC object and then the AUC.
    # We use test_data$incident_case as the true outcome.
    roc_obj <- roc(test_data$incident_case, predictions)
    AUC_results_1[disease, cluster] <- auc(roc_obj)
  }
}

colnames(AUC_results_1) <- c('1', '2', '3', '4', '5', '6', '7')

# Print the resulting AUC matrix
print(AUC_results_1)



# AUC Heatmap -------------------------------------------------------------

heat_colors <- colorRampPalette(c("dodgerblue", "white", "brown2"))(200)

min_val <- min(AUC_results_1, na.rm = TRUE)
max_val <- max(AUC_results_1, na.rm = TRUE)

#Create a breaks vector that forces 0.5 to be in the middle of the scale.
# This only works properly if your data spans below and above 0.5.
if(min_val < 0.5 & max_val > 0.5){
  # Create two sequences: one from min_val to 0.5, and one from 0.5 to max_val.
  # We use 51 values in each so that, after removing the duplicate center value, we have 101 breakpoints.
  breaks <- c(seq(min_val, 0.5, length.out = 101),
              seq(0.5, max_val, length.out = 101)[-1])
} else {
  # If your data do not span across 0.5, just use a regular sequence.
  breaks <- seq(min_val, max_val, length.out = 201)
}

heatmap.2(AUC_results_1,
          col = heat_colors,
          trace = "none", 
          # Remove unnecessary trace lines
          density.info = "none",       # No density plot
          margins = c(5, 15),          # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",              # Remove extra key title
          key.xlab = "AUC",        # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = c(expression(bold(AD) * " (450; 41,698)"),
                     expression(bold(PD) * " (1953; 39,518)"),
                     expression(bold(CKD) * " (1659; 40,429)"),
                     expression(bold(CAD) * " (11,129; 15,380)"),
                     expression(bold(Diabetes) * " (590; 41,646)")),      # Disease names as row labels
          labCol = c(expression(bold('1')),
                     expression(bold('2')),
                     expression(bold('3')),
                     expression(bold('4')),
                     expression(bold('5')),
                     expression(bold('6')),
                     expression(bold('7'))),
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(AUC_results_1, 3), # Add rounded OR values inside the heatmap
          notecol = "black",            # Text color for cell values
          notecex = 0.8,
          symbreaks = FALSE,
          symkey = FALSE,
          breaks = breaks,
          cexRow = 1.3)





# Logistic Regression with Exposures --------------------------------------

selected_vars <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_vars.rds")
hat_parameters <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/hat_parameters.rds")


# Define the diseases (desired labels) and clusters.
clusters <- paste0("cluster_", 1:7)

diseases <- c("AD", "PD", "CKD", "CAD", "Diabetes")
# Create empty matrices to store results.
# Rows correspond to clusters (1 through 7), and columns to diseases.
logistic_cluster_result <- matrix(NA, nrow = 7, ncol = length(diseases))
colnames(logistic_cluster_result) <- diseases
rownames(logistic_cluster_result) <- clusters

p_value_result <- matrix(NA, nrow = 7, ncol = length(diseases))
colnames(p_value_result) <- diseases
rownames(p_value_result) <- clusters

# --- Loop through each disease and cluster ---
for(disease in diseases) {
  
  # Retrieve the disease data frame.
  # Assuming that in your list the names are in lower-case with _df suffix.
  # For example, for "AD" you have disease_df_list[["ad_df"]]
  df <- disease_df_list[[ paste0(tolower(disease), "_df") ]]
  
  # Loop over clusters 1 to 7.
  for(j in 1:7) {
    current_cluster <- paste0("cluster_", j)
    
    # Look up the exposures selected for the current cluster from selected_vars.
    # This returns a character vector of exposure names with a value >= 0.97.
    selected_exposures <- rownames(selected_vars)[ selected_vars[, current_cluster] >= 0.97 ]
    
    # Keep only those exposures that are present in the data frame.
    selected_exposures <- intersect(selected_exposures, colnames(df))
    
    # If at least one exposure is selected, build the formula and run logistic regression.
    if(length(selected_exposures) > 0) {
      # Build the formula string. For example: "incident_case ~ water_1000m + no2_2010"
      formula_str <- paste("incident_case ~", paste(selected_exposures, collapse = " + "))
      
      # Fit the logistic regression model.
      model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = df)
      
      # Composite Odds Ratio:
      # Here we take the sum of coefficients for all exposures (ignoring the intercept)
      # and exponentiate that sum to get a single odds ratio.
      composite_log_coef <- sum(coef(model)[-1])
      composite_odds_ratio <- exp(composite_log_coef)
      logistic_cluster_result[current_cluster, disease] <- composite_odds_ratio
      
      # Overall p-value:
      # Compare the full model to an intercept-only model using a likelihood ratio test.
      null_model <- glm(incident_case ~ 1, family = binomial(link = "logit"), data = df)
      lrt <- anova(null_model, model, test = "LRT")
      overall_p <- lrt$`Pr(>Chi)`[2]  # second row corresponds to the full model
      # Store the p-value only if it is < 0.05; otherwise, leave it as NA.
      p_value_result[current_cluster, disease] <- ifelse(overall_p < 0.05, overall_p, NA)
    } 
    }
  
}

# --- Display Results ---
cat("Composite Odds Ratios (by cluster and disease):\n")
print(logistic_cluster_result)

cat("\nOverall p-values (if < 0.05):\n")
print(p_value_result)


# Testing formula ---------------------------------------------------------


# Define diseases and clusters vectors (adjust as needed)
diseases <- c("AD", "CKD", "CAD", "PD", "Diabetes")
clusters <- paste0("cluster_", 1:7)

# Create an empty character matrix to store the formula strings.
# Rows are clusters and columns are diseases.
formula_result <- matrix(NA, nrow = length(clusters), ncol = length(diseases),
                         dimnames = list(clusters, diseases))

# Loop over each disease and each cluster
for(disease in diseases) {
  # Retrieve the data frame for this disease.
  df <- disease_df_list[[disease]]
  
  for(j in 1:length(clusters)) {
    current_cluster <- clusters[j]
    
    # Determine which exposures are "selected" for the current cluster.
    # This retrieves the exposures (rownames of selected_vars) with a value >= 0.97 in the current cluster column.
    selected_exposures <- rownames(selected_vars)[ selected_vars[, current_cluster] >= 0.97 ]
    
    # Intersect with the columns in the current data frame (only keep exposures that actually exist in df)
    selected_exposures <- intersect(selected_exposures, colnames(df))
    
    # If at least one exposure is selected, create the formula string.
    if(length(selected_exposures) > 0) {
      formula_str <- paste("incident_case ~", paste(selected_exposures, collapse = " + "))
    } else {
      formula_str <- NA  # or you can leave it as an empty string if preferred
    }
    
    # Store the formula string in the results matrix.
    formula_result[current_cluster, disease] <- formula_str
  }
}

# Print the resulting formulas.
print(formula_result)




formula_list <- list(
  pred_1 = "incident_case ~ no2_2010 + inv_dis_maj_road + water_1000m + met_score",
  pred_2 = "incident_case ~ no2_2010 + pm10 + pm2.5 + traffic_intensity + greenspace_1000m + water_1000m + met_score",
  pred_3 = "incident_case ~ neuro_score + pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + coast_distance + sleep_data + met_score + anxiety_Yes",
  pred_4 = "incident_case ~ pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + sleep_data + diet_score",
  pred_5 = "incident_case ~ neuro_score + pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + sleep_data + diet_score",
  pred_6 = "incident_case ~ neuro_score + pack_years + no2_2010 + pm10 + pm2.5 + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + met_score + `vehicles_household_Two or more`",
  pred_7 = "incident_case ~ neuro_score + pack_years + no2_2010 + pm2.5 + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + smoking_status_Previous"
)



# Define diseases and clusters
diseases <- names(disease_df_list)  # "AD", "PD", "CKD", "CAD", "Diabetes"
clusters <- paste0("cluster_", 1:7)

# Create empty result matrices.
# Rows: clusters; Columns: diseases.
logistic_cluster_result <- matrix(NA, nrow = length(clusters), ncol = length(diseases),
                                  dimnames = list(clusters, diseases))
p_value_result <- matrix(NA, nrow = length(clusters), ncol = length(diseases),
                         dimnames = list(clusters, diseases))

# Loop over each disease and each cluster
for(disease in diseases) {
  # Retrieve the disease data frame.
  df <- disease_df_list[[disease]]
  
  # (Optional: Insert your train/test split here. For simplicity, we use the full df.)
  train_data <- df
  # Loop over clusters 1 to 7
  for(j in 1:length(clusters)) {
    current_cluster <- clusters[j]
    # Get the formula for the current cluster from the formula_list.
    # For example, for j==1, use formula_list$pred_1.
    formula_str <- formula_list[[ paste0("pred_", j) ]]
    
    # Fit logistic regression using the selected formula.
    model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = train_data)
    
    # Compute a composite odds ratio:
    # Sum the coefficients for all predictors (ignoring the intercept),
    # then exponentiate that sum.
    composite_log_coef <- sum(coef(model)[-1])
    composite_odds_ratio <- exp(composite_log_coef)
    logistic_cluster_result[current_cluster, disease] <- composite_odds_ratio
    
    # Compute an overall p-value via a likelihood ratio test.
    null_model <- glm(incident_case ~ 1, family = binomial(link = "logit"), data = train_data)
    lrt <- anova(null_model, model, test = "LRT")
    overall_p <- lrt$`Pr(>Chi)`[2]  # The p-value for the full model
    # Save the p-value if it is less than 0.05; otherwise, leave as NA.
    p_value_result[current_cluster, disease] <- ifelse(overall_p < 0.05, overall_p, NA)
  }
}

# View the results:
cat("Composite Odds Ratios:\n")
print(logistic_cluster_result)

cat("\nSignificant p-values (< 0.05):\n")
print(p_value_result)



# AUC  --------------------------------------------------------------------

formula_list <- list(
  pred_1 = "incident_case ~ no2_2010 + inv_dis_maj_road + water_1000m + met_score",
  pred_2 = "incident_case ~ no2_2010 + pm10 + pm2.5 + traffic_intensity + greenspace_1000m + water_1000m + met_score",
  pred_3 = "incident_case ~ neuro_score + pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + coast_distance + sleep_data + met_score + anxiety_Yes",
  pred_4 = "incident_case ~ pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + sleep_data + diet_score",
  pred_5 = "incident_case ~ neuro_score + pack_years + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + sleep_data + diet_score",
  pred_6 = "incident_case ~ neuro_score + pack_years + no2_2010 + pm10 + pm2.5 + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + met_score + `vehicles_household_Two or more`",
  pred_7 = "incident_case ~ neuro_score + pack_years + no2_2010 + pm2.5 + traffic_intensity + inv_dis_maj_road + greenspace_1000m + water_1000m + multiple_deprivation_index + smoking_status_Previous"
)

disease_df_list <- list(
  AD = ad_df,
  PD = pd_df,
  CKD = ckd_df,
  CAD = cad_df,
  
  Diabetes = diabetes_df
)


# Define diseases and clusters vectors
diseases <- c("AD", "PD", "CKD", "CAD",  "Diabetes")
clusters <- c("cluster_1", "cluster_2", "cluster_3", "cluster_4", "cluster_5", "cluster_6", "cluster_7")

# Create an empty matrix to store AUC values.
AUC_results <- matrix(NA, nrow = length(diseases), ncol = length(clusters),
                      dimnames = list(diseases, clusters))

# Loop over each disease data frame
for (disease in diseases) {
  # Get the data frame for this disease from your list
  df <- disease_df_list[[disease]]
  
  # (Optional) If you have a separate train/test split, do it here.
  # For this example, we'll use the full data frame as both train and test.
  set.seed(1342)
  train <- sample(1:nrow(df), floor(0.8 * nrow(df)))
  test <- seq(1, nrow(df))[-train]
  
  
  train_data <- df[train, ]
  test_data  <- df[-train, ]
  
  # Loop over each cluster dummy variable
  for (i in 1:7) {
    # Build the logistic regression formula dynamically.
    # For example: "incident_case ~ cluster_1"
    formula_str <- formula_list[[ paste0("pred_", i) ]]
    
    # Fit the logistic regression model
    model <- glm(as.formula(formula_str), family = binomial(link = "logit"), data = train_data)
    
    # Get predicted probabilities on the test data
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Compute the ROC object and then the AUC.
    # We use test_data$incident_case as the true outcome.
    roc_obj <- roc(test_data$incident_case, predictions)
    AUC_results[disease, clusters[i]] <- auc(roc_obj)
  }
}

colnames(AUC_results) <- c('1', '2', '3', '4', '5', '6', '7')

# Print the resulting AUC matrix
print(AUC_results)
setwd('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Analysis')
saveRDS(AUC_results, file = 'AUC_results_exposures.rds')


# AUC Plot ----------------------------------------------------------------

heat_colors <- colorRampPalette(c('white', "brown2"))(100)

#min_val <- min(AUC_results, na.rm = TRUE)
#max_val <- max(AUC_results, na.rm = TRUE)

#Create a breaks vector that forces 0.5 to be in the middle of the scale.
# This only works properly if your data spans below and above 0.5.
#if(min_val < 0.5 & max_val > 0.5){
  # Create two sequences: one from min_val to 0.5, and one from 0.5 to max_val.
  # We use 51 values in each so that, after removing the duplicate center value, we have 101 breakpoints.
#  breaks <- c(seq(min_val, 0.5, length.out = 51),
  #            seq(0.5, max_val, length.out = 51)[-1])
#} else {
  # If your data do not span across 0.5, just use a regular sequence.
 # breaks <- seq(min_val, max_val, length.out = 101)
#}

heatmap.2(AUC_results,
          col = heat_colors,
          trace = "none", 
          # Remove unnecessary trace lines
          density.info = "none",       # No density plot
          margins = c(5, 15),          # Adjust margins for better spacing
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",              # Remove extra key title
          key.xlab = "AUC",        # Label the scale
          key.par = list(mar = c(5, 1, 2, 4)), 
          labRow = c(expression(bold(AD) * " (450; 41,698)"),
                     expression(bold(PD) * " (1953; 39,518)"),
                     expression(bold(CKD) * " (1659; 40,429)"),
                     expression(bold(CAD) * " (11,129; 15,380)"),
                     expression(bold(Diabetes) * " (590; 41,646)")),      # Disease names as row labels
          labCol = c(expression(bold('1')),
                     expression(bold('2')),
                     expression(bold('3')),
                     expression(bold('4')),
                     expression(bold('5')),
                     expression(bold('6')),
                     expression(bold('7'))),
          Rowv = FALSE,
          Colv = FALSE,
          cellnote = round(AUC_results, 3), # Add rounded OR values inside the heatmap
          notecol = "black",            # Text color for cell values
          notecex = 0.8,
          symbreaks = FALSE,
          symkey = FALSE,
          cexRow = 1.3)




# Merging disease with dataframe and NA removal ---------------------------



ad_df_new <- merged_data %>% left_join(ad %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

ckd_df_new <- merged_data %>% left_join(ckd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

cad_df_new <- merged_data %>% left_join(cad %>% dplyr::select(eid, incident_case), by = c("id" = "eid"))

pd_df_new <- merged_data %>% left_join(pd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

diabetes_df_new <- merged_data %>% left_join(diabetes %>% dplyr::select(eid, incident_case), by = c("id" = "eid"))


disease_df_list <- list(
  ad_df = ad_df_new,
  ckd_df = ckd_df_new,
  cad_df = cad_df_new,
  pd_df = pd_df_new,
  diabetes_df = diabetes_df_new
)

new_list <- list()
for (i in seq_along(disease_df_list)) {
  new_list[[i]] <- na.omit(disease_df_list[[i]])
}

ad_df_new <- new_list[[1]]
cad_df_new <- new_list[[2]]
ckd_df_new <- new_list[[3]]
diabetes_df_new <- new_list[[4]]
pd_df_new <- new_list[[5]]

disease_df_list <- list(
  ad_df = ad_df_new,
  ckd_df = ckd_df_new,
  cad_df = cad_df_new,
  pd_df = pd_df_new,
  diabetes_df = diabetes_df_new
)



# Incidence plots ---------------------------------------------------------

disease_df_list <- list(
  AD = ad_df_new,
  PD = pd_df_new,
  CKD = ckd_df_new,
  CAD = cad_df_new,
  Diabetes = diabetes_df_new
)

disease_sample_size <- c(
  AD = 41698,
  PD = 39518,
  CKD = 40429,
  CAD = 15380,
  Diabetes = 41646
)


disease_df_list_norm <- lapply(names(disease_df_list), function(dname) {
  df <- disease_df_list[[dname]]
  # Use dname directly as the disease name
  disease_name <- dname
  sample_size <- disease_sample_size[disease_name]
  df %>% mutate(normalized_incidence = incident_case / sample_size)
})
# Preserve the original names.
names(disease_df_list_norm) <- names(disease_df_list)

# 2. For each normalized data frame, aggregate by 'cluster' (assumes a column "cluster" exists).
#    We compute the mean normalized incidence for each cluster.
agg_list <- lapply(disease_df_list_norm, function(df) {
  df %>%
    group_by(cluster) %>%
    summarise(mean_norm = mean(normalized_incidence, na.rm = TRUE)) %>%
    arrange(cluster)
})

# 3. Combine the aggregated results into one data frame:
#    Each row will represent a cluster and each column a disease.
clusters <- agg_list[[1]]$cluster  # assume all diseases have the same cluster values
df_combined <- data.frame(cluster = clusters)

for(disease in names(agg_list)) {
  temp_df <- agg_list[[disease]] %>% rename(!!disease := mean_norm)
  df_combined <- left_join(df_combined, temp_df, by = "cluster")
}

# 4. Convert the combined data frame into a matrix.
#    We want diseases as rows and clusters as columns.
df_matrix <- as.matrix(df_combined[, -1])  # remove the 'cluster' column
df_matrix <- t(df_matrix)  # transpose: rows = diseases, columns = clusters

# Set proper row and column names.
rownames(df_matrix) <- names(agg_list)             # diseases as row names
colnames(df_matrix) <- as.character(df_combined$cluster)  # clusters as column names

# (Optional) Order the columns numerically.
df_matrix <- df_matrix[, order(as.numeric(colnames(df_matrix)))]

# 5. (Optional) Scale the matrix row-wise (convert each disease's values to z-scores).
df_matrix_scaled <- t(apply(df_matrix, 1, function(x) (x - mean(x)) / sd(x)))
new_order <- c("AD", "PD", "CKD", "CAD", "Diabetes")

# Reorder the matrix rows
df_matrix_scaled <- df_matrix_scaled[new_order, ]


# For each disease data frame, aggregate the incidence counts by cluster.
agg_list <- lapply(disease_df_list, function(df) {
  df %>%
    group_by(cluster) %>%
    summarise(total_incidence = sum(incident_case, na.rm = TRUE)) %>%
    arrange(cluster)
})

# Get the cluster numbers (assume all diseases have the same clusters)
clusters <- agg_list[[1]]$cluster

# Start with a data frame of cluster numbers.
df_incidence_count <- data.frame(cluster = clusters)

# Loop over each disease and join its aggregated counts into df_incidence_count.
for (disease in names(agg_list)) {
  temp_df <- agg_list[[disease]] %>% rename(!!disease := total_incidence)
  df_incidence_count <- left_join(df_incidence_count, temp_df, by = "cluster")
}

df_incidence_count <- t(df_incidence_count)
df_incidence_count <- df_incidence_count[-1,]

# 6. Generate a heatmap using heatmap.2.
heat_colors <- colorRampPalette(c('white', "brown2"))(200)

# Open a new graphics device (optional)
dev.new(width = 20, height = 10)
par(mar = c(2,0.5,3,9))

heatmap.2(df_matrix_scaled, 
          scale = "none",                # Already scaled manually
          col = heat_colors,             # Use the defined palette
          trace = "none",                # No trace lines
          density.info = "none",         # No density plot
          margins = c(5, 15),            # Adjust margins
          key = TRUE,
          dendrogram = 'none', 
          key.title = "",                # No extra key title
          key.xlab = "Standardised Incidence Rate",  # Label for the color key
          key.par = list(mar = c(5, 2, 2, 4)),
          labRow = c(expression(bold(AD) * " (450; 41,698)"),
                     expression(bold(PD) * " (1953; 39,518)"),
                     expression(bold(CKD) * " (1659; 40,429)"),
                     expression(bold(CAD) * " (11,129; 15,380)"),
                     expression(bold(Diabetes) * " (590; 41,646)")),      # Disease names as row labels
          labCol = c(expression(bold('1')),
                     expression(bold('2')),
                     expression(bold('3')),
                     expression(bold('4')),
                     expression(bold('5')),
                     expression(bold('6')),
                     expression(bold('7'))),
          Rowv = FALSE,
          Colv = FALSE,
          symbreaks = TRUE,
          symkey = FALSE,
          cellnote = round(df_incidence_count, 2), # Display scaled values in cells
          notecol = "black",
          notecex = 0.8,
          cexRow = 1.3)




# Summing diseases --------------------------------------------------------

for (disease in disease_df_list){
  print(sum(disease$incident_case))
}
