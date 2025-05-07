# ------------------------------------------------------------------------------
# Purpose: This script runs stability selection LASSO for exposures and saves 
# calibration and selection proportion plots for each cluster
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(tidyverse)
library(ggrepel)
library(ggplot2)
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))
ukb_analysis_LASSO <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis_LASSO.rds")
X <- ukb_analysis_LASSO[,2:48]

# Selection, training and test data ============================================
set.seed(1342)
train <- sample(1:nrow(ukb_analysis_LASSO), floor(0.8 * nrow(ukb_analysis_LASSO)))
test <- seq(1, nrow(ukb_analysis_LASSO))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542

# Save info in summary tables ==================================================
hat_parameters <- matrix(data = NA, ncol = 7, nrow = 2)
colnames(hat_parameters) <- colnames(ukb_analysis_LASSO[,49:55])
rownames(hat_parameters) <- c("lambda", "pi")

selected_vars <- matrix(data = NA, ncol = 7, nrow = 44)
colnames(selected_vars) <- colnames(ukb_analysis_LASSO[,49:55])
rownames(selected_vars) <- colnames(ukb_analysis_LASSO[,c(3,4,7:48)])

for(i in 1:7){
  set.seed(3142)
  Y <- ukb_analysis_LASSO[,48+i]
  out <- VariableSelection(xdata = as.matrix(X)[select,], ydata = as.matrix(Y)[select],
                           verbose = FALSE, penalty.factor = c(0,1,1,0,0, rep(1, 42)), 
                           family = "binomial", n_cat = 3, seed = 123)
  
  # Save the calibration plot for each
  plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_Visuals/Calibration_Plots/C'
  plot_name <- paste(plot_name, i, '_calibration.png', sep="")
  png(plot_name, width = 800, height = 600)
  par(mar = c(7,5,6,6))
  CalibrationPlot(out)
  dev.off()
  # Extract the selection proportions
  selprop <- SelectionProportions(out)
  # Extract lambda, pi
  hat_params <- Argmax(out)
  
  # Save selection proportion plots
  plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_Visuals/Selection_Proportion_Plots/C'
  plot_name <- paste(plot_name, i, '_selection.png', sep="")
  png(plot_name, width = 800, height = 600)
  par(mar = c(10, 5, 1, 1))
  plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
       ylab = "Selection Proportion", xaxt = "n", 
       col = ifelse(selprop >= hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
  abline(h = hat_params[2], lty = 2, col = "darkred")
  for (k in 1:length(selprop)) {
    axis(side = 1, at = k, labels = names(selprop)[k],
         las = 2, col = ifelse(selprop[k] >= hat_params[2],
                               yes = "red", no = "grey"), 
         col.axis = ifelse(selprop[k] >= hat_params[2], yes = "red", no = "grey"))
  }
  dev.off()
  
  hat_parameters[1,i] <- Argmax(out)[1]
  hat_parameters[2,i] <- Argmax(out)[2]
  for(j in 1:44){
    selprop2 <- as.data.frame(SelectionProportions(out))
    selected_vars[j,i] <- selprop2[j,1]
  }
}

saveRDS(selected_vars, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_vars.rds")
saveRDS(hat_parameters, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/hat_parameters.rds")
