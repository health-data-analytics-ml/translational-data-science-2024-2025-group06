# ------------------------------------------------------------------------------
# Purpose: This script attempts to detect outliers through PCA (not used as final
# outlier technique)
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(missMDA)
library(FactoMineR)
library(ggcorrplot)
library(corrr)
library(mt)
ukb_missing_removed <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

# Just select numeric columns
numeric_cols <- names(ukb_missing_removed)[sapply(ukb_missing_removed, function(x) is.numeric(x) | is.integer(x))]
outlier_pca <- ukb_missing_removed[,numeric_cols]

# Remove NA values, reduces to 26099
outlier_pca <- na.omit(outlier_pca)

# Scale the columns
outlier_pca <- data.frame(scale(outlier_pca))

# Run PCA (princomp needs NA removed) ==========================================
data.pca <- princomp(outlier_pca)
summary(data.pca) # Takes 12/15 PCs to get 95% of variance explained
data.pca$loadings[, 1:2]
data.pca.scores <- data.pca$scores

# Just finds outliers of PC1 and PC2, 0.975 
data.pca.2 <- pca.outlier(outlier_pca, center = TRUE, scale=TRUE, conf.level = 0.975) 
data.pca.2$outlier

# 0.99
data.pca.3 <- pca.outlier(outlier_pca, center = TRUE, scale=TRUE, conf.level = 0.99) 

# Run missMDA ==================================================================
# Toy dataset
toy <- outlier_pca[1:1000,]
nb <- estim_ncpPCA(toy, method.cv = "Kfold", verbose = FALSE) # estimate the number of components from incomplete data
#(available methods include GCV to approximate CV)
nb$ncp #5
plot(0:5, nb$criterion, xlab = "nb dim", ylab = "MSEP")
res.comp <- imputePCA(toy, ncp = nb$ncp) # iterativePCA algorithm
res.comp$completeObs[1:3,]

res.pca <- PCA(res.comp, quanti.sup = 1, quali.sup = 12, ncp = nb$ncp, graph=FALSE)
plot(res.pca, hab=12, lab="quali");
plot(res.pca, choix="var")
