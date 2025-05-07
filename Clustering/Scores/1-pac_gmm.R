# ------------------------------------------------------------------------------
# Purpose: This script calculates the PAC score for GMM clustering
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(diceR)
library(cluster)
library(mclust)
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
toy_data <- data[1:5000, ]

# Pac Score for GMM ============================================================
consensus_gmm <- consensus_cluster(data, nk = 10, algorithm = 'gmm')
gap_pac <- PAC(consensus_gmm)
print(gap_pac)
