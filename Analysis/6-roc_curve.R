rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(pheatmap)
suppressPackageStartupMessages(library(ROCR))

# Data ==========================================================================
ukb_analysis_LASSO <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis_LASSO.rds")
set.seed(1342)
train <- sample(1:nrow(ukb_analysis_LASSO), floor(0.8 * nrow(ukb_analysis_LASSO)))
test <- seq(1, nrow(ukb_analysis_LASSO))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542
rm(ukb_analysis_LASSO)
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
ukb_analysis_test <- ukb_analysis[test,]


# Run logistic regression for each cluster on test data ========================
roc_data <- data.frame()
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
  perf.auc <- performance(pred.obj, "auc")
  score.auc <- round(as.numeric(perf.auc@y.values), 3)
  cluster_data <- data.frame(
    FPR = fpr,
    TPR = tpr,
    AUC = score.auc,
    Cluster = paste0("Cluster ", i),
    Cluster_AUC = paste0("Cluster ", i, " (AUC = ", score.auc, ")")
  )
  
  # Append to the main data frame
  roc_data <- bind_rows(roc_data, cluster_data)
}

# ROC Plot for all clusters, format like univariate plots ======================
colors <- c("#999999", "#984EA3", "#4DAF4A", "#E41A1C", "#FFFF33", "#377EB8", "#A65628", "#F781BF")
roc_curves <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Cluster_AUC, group = Cluster)) +
  geom_line(size = 0.6) +
  scale_color_manual(values = colors) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(colour = 'grey95'),
    panel.grid.major.y = element_line(colour = 'grey95'),
    panel.grid.minor.y = element_line(colour = 'grey95'),
    panel.background = element_rect(fill = 'grey99'),
    plot.background = element_rect(fill = 'grey99')) +
  labs(
    title = "ROC Curves (Exposures)",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)",
    color = 'Cluster') +
  # theme(legend.position = 'right')
  theme(
    legend.position = c(.98, .3),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6))
roc_curves

file_path <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/roc_curves.png'
ggsave(file_path, plot = roc_curves, width = 8, height = 8, dpi = 300)


