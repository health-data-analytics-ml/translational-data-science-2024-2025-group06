rm(list = ls())
library(dplyr)

# Data split ===================================================================
## Sex, age, ethnicity, proteins, clusters
ukb_proteins_lasso <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_lasso.rds")
set.seed(1342)
train <- sample(1:nrow(ukb_proteins_lasso), floor(0.8 * nrow(ukb_proteins_lasso)))
test <- seq(1, nrow(ukb_proteins_lasso))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542

# Create correct equation for logistic regression ==============================
## Get outputs from stability selection lasso
## Change from IMMI once I have final numbers
selected_vars <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_selected_vars.rds")
selected_vars <- as.data.frame(selected_vars)
hat_parameters <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_hat_parameters.rds")
hat_parameters <- as.data.frame(hat_parameters)

# 1 for selected, 0 for not selected, based on pi value for each cluster
selected_p <- selected_vars
for (i in 1:7) {
  cluster <- paste0('cluster_', i)
  selected_p[[cluster]] <- ifelse(selected_p[[cluster]] > hat_parameters['pi', cluster], 1, 0)
}
saveRDS(selected_p, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/selected_proteins.rds")

# Set values to rowname if 1, to NA if 0
selected_p <- as.data.frame(Map(function(col, row_names) ifelse(col == 1, row_names, NA),
                                selected_p,
                                list(rownames(selected_p))))

# Make the string var1 + var2 + ....
logistic_variables <- matrix(data = NA, ncol = 1, nrow = 7)
colnames(logistic_variables) <- c("Variables")
rownames(logistic_variables) <- colnames(selected_p)[1:7]

for (i in 1:7){
  vars <- unique(na.omit(selected_p[,i]))
  c1_vars <- vars[1]
  for (k in 2:length(vars)){
    c1_vars <- paste(c1_vars, vars[k], sep = " + ")
  }
  logistic_variables[i,1] <- c1_vars
}
logistic_variables <- as.data.frame(logistic_variables)
logistic_variables$Variables <- paste(logistic_variables$Variables, '+ age + sex + ethnic_background')

# Logistic regression ==========================================================
## Split data
ukb_proteins_train <- ukb_proteins_lasso[train,]

# Create empty df to save odds ratio, p-value
log_output_b <- data.frame(odds_ratio = numeric(),
                           p_value = numeric(), 
                           cluster = character(),
                           variable = character(),
                           stringsAsFactors=FALSE)

# Create models for all clusters ===============================================
models <- list()
# ukb_proteins_train <- ukb_proteins_train[1:1000,]
for (i in 1:7) {
  cluster <- rownames(logistic_variables)[i]
  formula_str <- logistic_variables$Variables[i]
  formula <- as.formula(paste(cluster, '~', formula_str))
  model_glm <- glm(formula, data = ukb_proteins_train, family = 'binomial')
  models[[cluster]] <- model_glm
  # Save each model as rds file
  output_path <- paste0("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/logistic_", cluster, '.rds')
  saveRDS(model_glm, output_path)
  # Store values
  log_output <- cbind('odds_ratio' = round(exp(coef(model_glm))[-1], digits = 2),
                      'p_value' = round(coef(summary(model_glm))[-1,4], digits = 5),
                      'cluster' = rep(cluster, length(coef(summary(model_glm))[,1])-1))
  log_output_a <- as.data.frame(log_output) %>% mutate(Variable = rownames(log_output))
  n <- dim(log_output_a)[1]
  log_output_a <- log_output_a[1:(n-3),]
  log_output_b <- rbind(log_output_b, log_output_a)
}

saveRDS(log_output_b, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/log_output.rds")










