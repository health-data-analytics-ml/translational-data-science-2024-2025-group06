# Runs univariate analysis on proteins and generates tables to use in proteins_univariate_visualisations.R
rm(list = ls())

# Univariate analysis ==========================================================
# 100% of data used
ukb_proteins_univariate <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_univariate.rds")

# Function for logistic regression, grab p-value
protein_indices <- 4:1346
logistic <- function(protein_col, cluster) {
  formula <- as.formula(paste(cluster, '~', names(ukb_proteins_univariate)[protein_col], '+ age + sex + ethnic_background'))
  model <- glm(formula, data = ukb_proteins_univariate, family = 'binomial')
  # model <- glm(cluster ~ ukb_proteins_univariate[[protein_col]] + age + sex + ethnic_background, 
  #              data = ukb_proteins_univariate, family = 'binomial')
  coef_summary <- summary(model)$coefficients
  data.frame(protein = names(ukb_proteins_univariate)[protein_col],
             estimate = coef_summary[2, 1],
             p_value = coef_summary[2, 4])
}

# Apply function to each cluster separately
cluster_vars <- paste0('cluster_', 1:7)
for (cluster in cluster_vars) {
  # do.call - list of dfs and combines row wise
  # function(proteins) to make sure to apply the function to each protein
  results <- do.call(rbind, lapply(protein_indices, function(protein) logistic(protein, cluster)))
  output_path <- paste0("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/univariate_", cluster, '.rds')
  saveRDS(results, output_path)
}