library(tidyverse)

ad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Alzheimer/Outputs/output_final.rds')
ckd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Chronic_Kidney_Disease/Outputs/output_final.rds')
cad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Coronary_Artery_Disease/Outputs/output_final.rds')
diabetes <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Diabetes/Outputs/output_final.rds')
pd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Parkinson/Outputs/output_final.rds')

ukb_final_reduced <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')
data_read <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds') 

ukb_imputed <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')
ukb_analysis <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds')


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
  pd_df = pd_df,
  ckd_df = ckd_df,
  cad_df = cad_df,
  diabetes_df = diabetes_df
)


# Removing NAs ------------------------------------------------------------
new_list <- list()
for (i in seq_along(disease_df_list)) {
  new_list[[i]] <- na.omit(disease_df_list[[i]])
}

ad_df <- new_list[[1]]
pd_df <- new_list[[2]]
ckd_df <- new_list[[3]]
cad_df <- new_list[[4]]
diabetes_df <- new_list[[5]]



disease_df_list <- list(
  AD = ad_df,
  PD = pd_df,
  CKD = ckd_df,
  CAD = cad_df,
  Diabetes = diabetes_df
)



# Adding Age, Sex, and Ethnic Background ----------------------------------
ukb_analysis <- ukb_analysis %>% 
  tibble::rownames_to_column(var = "id")


for (i in seq_along(disease_df_list)){
  df <- disease_df_list[[i]]
  
  df <- df %>%
    left_join(ukb_analysis %>% dplyr::select(id, sex, ethnic_background), 
              by = 'id')
  
  # Update the list with the modified dataframe
  disease_df_list[[i]] <- df
  
}

ad_df <- disease_df_list[[1]]
pd_df <- disease_df_list[[2]]
ckd_df <- disease_df_list[[3]]
cad_df <- disease_df_list[[4]]
diabetes_df <- disease_df_list[[5]]



# Adding age and proteins -------------------------------------------------




# Logistic regression 1 -----------------------------------------------------

results_matrix <- matrix(NA, nrow = 5, ncol = 2)
rownames(results_matrix) <- c('AD', 'PD', 'CKD', 'CAD', 'Diabetes')
colnames(results_matrix) <- c("cluster_result", "pvalue")

# Loop over each disease data frame.
for(i in seq_along(disease_df_list)) {
  df <- disease_df_list[[i]]

  
  # Fit the logistic regression model.
  model <- glm(incident_case ~ as.factor(cluster) + age + sex + ethnic_background, 
               family = binomial(link = "logit"), data = df)
  
  # Extract the odds ratio for the cluster variable (assumed to be the second coefficient).
  odds_ratio <- exp(coef(model)[2])
  
  # Extract the p-value for the cluster variable.
  p_val <- summary(model)$coefficients[2, "Pr(>|z|)"]
  
  # Store the odds ratio in the matrix.
  results_matrix[i, "cluster_result"] <- odds_ratio
  
  # Store the p-value only if it's less than 0.05; otherwise, it remains NA.
  results_matrix[i, "pvalue"] <- p_val
}

# View the results matrix.
results_matrix


# AUC 1 -------------------------------------------------------------------

library(pROC)
disease <- c("AD", "PD", "CKD", "CAD", "Diabetes")
AUC_results_1 <- matrix(NA, nrow = 5, ncol = 1, 
                        dimnames = list(disease,
                                        c('AUC')))


for(i in seq_along(disease_df_list)) {
  df <- disease_df_list[[i]]
  
  df <- df %>% filter(ethnic_background != "Mixed")
  
  set.seed(1342)
  train <- sample(1:nrow(df), floor(0.8 * nrow(df)))
  test <- seq(1, nrow(df))[-train]
  
  train_data <- df[train, ]
  test_data  <- df[-train, ]
  model <- glm(incident_case ~ as.factor(cluster) + age + sex + ethnic_background, 
               family = binomial(link = "logit"), data = train_data)
# Get predicted probabilities on the test data
  predictions <- predict(model, newdata = test_data, type = "response")
    
    # Compute the ROC object and then the AUC.
    # We use test_data$incident_case as the true outcome.
  roc_obj <- roc(test_data$incident_case, predictions)
  AUC_results_1[disease[i], 'AUC'] <- auc(roc_obj)

}

AUC_results_1

# Joining proteins and disease df --------------------------------------------

proteins <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_lasso.rds")
selected_proteins <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Analysis/selected_proteins.rds')

# Making a list of the selected proteins
library(stringr)  # For str_split

all_proteins <- selected_proteins$Variables %>%
  str_split(" \\+ ") %>%
  unlist() %>%
  unique()



# Scaling Age -------------------------------------------------------------

for (i in seq_along(disease_df_list)){
  disease_df_list[[i]]$age <- as.numeric(scale(disease_df_list[[i]]$age))
}

ad_df <- disease_df_list[[1]]
pd_df <- disease_df_list[[2]]
ckd_df <- disease_df_list[[3]]
cad_df <- disease_df_list[[4]]
diabetes_df <- disease_df_list[[5]]


# I need to get IDs for disease and proteins and merge them into one dataset 
# with each disease 

# Then run a logistic regression for each dataset with 1) cluster + proteins (remember its not all the proteins)
# 2) cluster + protein + exposures 
# Calculate AUC for each one and show in a heatmap???

