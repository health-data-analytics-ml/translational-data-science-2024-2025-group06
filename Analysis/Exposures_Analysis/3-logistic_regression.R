# Same but with 7 clusters
rm(list = ls())

# Set up ============
## Library ==============
library(tidyverse)
library(ggrepel)
library(ggplot2)

# Logistic regression ===============
## Data -  - we want full categories =========
selected_vars <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_vars.rds")
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
hat_parameters <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/hat_parameters.rds")

## Select variables to use in logistic ===========
# Output is logistic_variables which has a string of var1 + var2 + var3

# All pi values are 0.99-0.97 (look at hat_parameters) 
# We can cutoff at 0.97 as this is value is only in the relevant cluster
selected <- ifelse(selected_vars >= 0.97, 1, 0)

# Add row names to column
# Delete the category name as we don't want this for logistic
selected <- as.data.frame(selected) %>% mutate(Variable = rownames(selected_vars))
selected$Variable[15:44] <- sub("_[^_]+$", "", selected$Variable[15:44])

# Set row value to be the category name
for(i in 1:7){
  for(j in 1:nrow(selected)){
    var1 <- selected[j,8]
    selected[j,i] <- ifelse(selected[j,i] == 1, var1, NA)
  }
}

saveRDS(selected, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_variables.rds")


# # Make the string var1 + var2 + ....
# logistic_variables <- matrix(data = NA, ncol = 1, nrow = 7)
# colnames(logistic_variables) <- c("Variables")
# rownames(logistic_variables) <- colnames(selected)[1:7]
# 
# for (i in 1:7){
#   vars <- unique(na.omit(selected[,i]))
#   c1_vars <- vars[1]
#   for (k in 2:length(vars)){
#     c1_vars <- paste(c1_vars, vars[k], sep = " + ")
#   }
#   logistic_variables[i,1] <- c1_vars
# }
# saveRDS(logistic_variables, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/logistic_variables.rds")
# # Done

# Logistic regression ==========
# Split data
# Data split ===================
## Load data - Selection, training and test data ==================
ukb_analysis_LASSO <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis_LASSO.rds")
set.seed(1342)
train <- sample(1:nrow(ukb_analysis_LASSO), floor(0.8 * nrow(ukb_analysis_LASSO)))
test <- seq(1, nrow(ukb_analysis_LASSO))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542
rm(ukb_analysis_LASSO)

# Load data ==============
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
selected_variables <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_variables.rds")
ukb_analysis_train <- ukb_analysis[train,]
ukb_analysis_test <- ukb_analysis[test,]

# Create empty df to save odds ratio, p-value
logistic_output <- data.frame(Odds_ratio = numeric(),
                           p_value = numeric(), 
                          cluster = character(),
                          Variable = character(),
                             stringsAsFactors=FALSE)

levels(ukb_analysis$smoking_status)
ukb_analysis$smoking_status <- relevel(ukb_analysis$smoking_status, ref='Previous')

for (i in 1:7){
  # Select cluster, variables from stability LASSO + demographics
  cluster <- colnames(ukb_analysis)[35+i]
  vars <- c(unique(na.omit(selected_variables[,i])), "age", "sex", "ethnic_background")
  # Train logistic regression and SAVE
  model_glm <- glm(ukb_analysis_train[,cluster] ~ ., data = ukb_analysis_train[, c(vars)], family = 'binomial')
  path <- '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/exposure_logistic/ExpLog_model_c'
  path <- paste(path, i, '.rds', sep = "")
  saveRDS(model_glm, path)
  
  # Save output into log_output_a and add to our full df (log_output_b)
  model_output <- cbind('Odds_ratio' = round(exp(coef(model_glm))[-1], digits = 2),
                        'p_value' = round(coef(summary(model_glm))[-1,4], digits = 5),
                        'cluster' = rep(cluster, length(coef(summary(model_glm))[,1])-1))
  log_output_a <- as.data.frame(model_output) %>% mutate(Variable = rownames(model_output))
  # Removes age, sex, ethnic background?
  # n <- dim(log_output_a)[1]
  # log_output_a <- log_output_a[1:(n-3),]
  logistic_output <- rbind(logistic_output, log_output_a)
}

logistic_output$p_value <- as.numeric(logistic_output$p_value)
logistic_output$Odds_ratio <- as.numeric(logistic_output$Odds_ratio)

saveRDS(logistic_output, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/logistic_output.rds")

# Test ====================
## Cluster 1 ================
#install.packages('ROCR')
suppressPackageStartupMessages(library(ROCR))

for(i in 1:7){
  # Select the saved logistic model output for cluster i
  path <- paste('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/exposure_logistic/ExpLog_model_c', i, '.rds', sep = "")
  model <- readRDS(path)
  # Predict
  model_pred <- predict(model, ukb_analysis_test, type = "response")
  # Create a dataset with true and predicted values
  compare <- data.frame(class = ukb_analysis_test[,35+i], probs = model_pred)
  # Plot ROC and AUC
  pred.obj = prediction(compare$probs, compare$class)
  perf.obj = performance(pred.obj, "tpr", "fpr")
  perf.auc <- performance(pred.obj, "auc")
  plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/ROC_curves/C'
  plot_name <- paste(plot_name, i, '_ROC.png', sep="")
  png(plot_name)
  plot(perf.obj, col = "blue")
  abline(a = 0, b = 1, lty = "dashed", col = "gray")
  text(0.9, 0.05, labels = paste0("AUC=", round(as.numeric(perf.auc@y.values),
                                                3)), col = "blue", cex = 0.7)
  dev.off()
}

# ROC Curve Layered
# Run logistic regression for each cluster on test data ========================
colors <- c("#999999", "#984EA3", "#4DAF4A", "#E41A1C", "#FFFF33", "#377EB8", "#A65628", "#F781BF")
roc_data <- list()
plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/roc_curve.png'
png(plot_name)
plot(0, 0, type='n', xlim = c(0,1), ylim = c(0,1),
     xlab = 'False Positive Rate (FPR)',
     ylab = 'True Positive Rate (TPR)',
     main = 'ROC Curves (Exposures)')
abline(a = 0, b = 1, lty = "dashed", col = "gray")
auc_values <- c()
for (i in 1:7){
  # Select the saved logistic model output for cluster i
  path <- paste('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/exposure_logistic/ExpLog_model_c', i, '.rds', sep = "")
  model <- readRDS(path)
  # Predict
  model_pred <- predict(model, ukb_analysis_test, type = "response")
  # Create a dataset with true and predicted values
  compare <- data.frame(class = ukb_analysis_test[,35+i], probs = model_pred)
  # Plot ROC curve
  pred.obj = prediction(compare$probs, compare$class)
  perf.obj = performance(pred.obj, "tpr", "fpr")
  fpr <- unlist(perf.obj@x.values)
  tpr <- unlist(perf.obj@y.values)
  lines(fpr, tpr, col = colors[i], lwd = 2)
  # Add AUC to plot
  perf.auc <- performance(pred.obj, "auc")
  score.auc <- round(as.numeric(perf.auc@y.values), 3)
  auc_values <- c(auc_values, paste('Cluster', i, '(AUC = ', score.auc, ')'))
}
legend('bottomright', legend = auc_values, col = colors, lwd = 2, cex = 0.8)
dev.off()


# Perfect separation ===========
# There's none :)

find_perfect_separation = function(df, target_col){
  ## This works for binary and continuous variables
  ### Would not work for categorical variables
  # output table
  out_table = as.data.frame(matrix(nrow = length(unique(target_col)),
                                   ncol = length(colnames(df))))
  colnames(out_table) = colnames(df)
  # looking at each feature
  for (col in colnames(df)){
    # looking at each cluster
    for (i in 1:length(unique(target_col))){
      # subsetting data into cluster i (cases) vs all other clusters (controls)
      cases = df[target_col == i, col]
      controls = df[target_col != i, col]
      if (length(unique(cases)) <= 2){ # binary variables
        if (length(unique(cases)) == 1){ # only one value = perfect separation
          out_table[i,col] = 1
        }
        if (length(unique(cases)) == 2){ # 2 values = no PS
          out_table[i,col] = 0
        }
        if (length(unique(cases)) < 1){ # this would be an error
          out_table[i,col] = NA
        }
      }
      
      if (length(unique(cases)) > 2){ # continuous variables
        if ((max(cases) <= min(controls)) | (max(controls) <= min(cases))){ # perfect and quasi separation
          out_table[i,col] = 1
          
        }
        
        if ((max(cases) > min(controls)) | (max(controls) > min(cases))){ # perfect and quasi separation
          
          out_table[i,col] = 0
          
        }
        
        else{out_table[i,col] = NA}
        
      }
    }
  }
  
  return(out_table)
}

# We want the data in its original scale, but with the categorical variables converted into binary values (one-hot encoding).
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')

str(ukb_final_reduced)
perfect_sep <- data.frame(cluster = gmm_model$classification, ukb_final_reduced)
perfect_sep <- perfect_sep %>% mutate(ethnic_background = ifelse((ethnic_background == "British" | 
                                                                      ethnic_background == "Black or Black British" |
                                                                      ethnic_background == "Asian or Asian British"), "British_ethnic_backgrounds", "Other_identified_ethnic_backgrounds"))
perfect_sep$ethnic_background <- as.factor(perfect_sep$ethnic_background)
factor_cols <- names(perfect_sep)[sapply(perfect_sep, is.factor)]
perfect_sep <- dummy_cols(perfect_sep, 
                                  select_columns = factor_cols,
                                  remove_first_dummy = FALSE,
                                  remove_selected_columns = TRUE)

all_exposures = colnames(perfect_sep)
all_exposures = all_exposures[all_exposures != 'cluster']

sep_df = find_perfect_separation(perfect_sep[,all_exposures], perfect_sep$cluster)

# Looking at all of the perfectly separating variables

reduced_sep_df = sep_df %>% select_if(colSums(.) != 0) # 

print(reduced_sep_df)
# data frame with 0 columns and 7 rows 


