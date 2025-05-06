rm(list = ls())
ukb_proteins_lasso <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_proteins_lasso.rds")

## Library =====================================================================
# library(tidyverse)
library(ggplot2)
library(ggrepel)
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))

### Toy dataset to test with 1000 rows
### ukb_proteins_lasso <- ukb_proteins_lasso[1:1000,]
x <- ukb_proteins_lasso[, 1:(ncol(ukb_proteins_lasso) - 7)]
proteins <- ukb_proteins_lasso[, 4:1346]

# Selection, training and test data
set.seed(1342)
train <- sample(1:nrow(ukb_proteins_lasso), floor(0.8 * nrow(ukb_proteins_lasso)))
test <- seq(1, nrow(ukb_proteins_lasso))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542

# Save info in summary tables ==================================================
hat_parameters <- matrix(data = NA, ncol = 7, nrow = 2)
colnames(hat_parameters) <- colnames(ukb_proteins_lasso[,1347:1353])
rownames(hat_parameters) <- c("lambda", "pi")

selected_vars <- matrix(data = NA, ncol = 7, nrow = ncol(proteins))
colnames(selected_vars) <- colnames(ukb_proteins_lasso[,1347:1353])
rownames(selected_vars) <- colnames(ukb_proteins_lasso[,4:1346])

# Run stability lasso for each cluster =======================================

for(i in 1:7){
  set.seed(3142)
  # colnames(ukb_proteins_lasso)[1346 + i]
  y <- ukb_proteins_lasso[,1346+i]
  out <- VariableSelection(
    xdata = as.matrix(x)[select,], 
    ydata = as.matrix(y)[select],
    verbose = FALSE,
    # First three predictors kept (age, sex, ethnicity)
    # The rest of the columns (proteins) are penalised
    penalty.factor = c(0, 0, 0, rep(1, ncol(proteins))),
    family = "binomial",
    n_cat = 3
  )
  path <- '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/out_C'
  path <- paste(path, i, '.rds', sep = "")
  saveRDS(out, path)
  
  # Save the calibration plot for each
  plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/calibration_plots/C'
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
  plot_name <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Protein_Analysis_Visuals/selection_proportion_plots/C'
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
  for(j in 1:1343){
    selprop2 <- as.data.frame(SelectionProportions(out))
    selected_vars[j,i] <- selprop2[j,1]
  }
}

saveRDS(selected_vars, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_selected_vars.rds")
saveRDS(hat_parameters, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Protein_Analysis/pls_hat_parameters.rds")









