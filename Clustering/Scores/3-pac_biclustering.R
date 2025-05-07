# ------------------------------------------------------------------------------
# Purpose: This script calculates the PAC score for biclustering
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
library(biclust)
library(diceR)
set.seed(42)
data <- as.matrix(readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds'))

# Perform biclustering using Plaid model
bc <- biclust(data, method = BCPlaid())

# Step 1: Initialize consensus matrices ========================================
num_iter <- 100  # Number of iterations
row_consensus <- matrix(0, nrow=nrow(data), ncol=nrow(data))
col_consensus <- matrix(0, nrow=ncol(data), ncol=ncol(data))

for (i in 1:num_iter) {
  temp_bc <- biclust(data, method = BCPlaid())  # Re-run biclustering
  
  row_clusters <- temp_bc@RowxNumber * 1  # Convert logical to numeric
  col_clusters <- temp_bc@NumberxCol * 1  # Convert logical to numeric
  
  # Ensure dimensions match before matrix multiplication
  if (nrow(row_clusters) == nrow(row_consensus)) {
    row_consensus <- row_consensus + (row_clusters %*% t(row_clusters))
  }
  
  if (nrow(col_clusters) == nrow(col_consensus)) {
    col_consensus <- col_consensus + (col_clusters %*% t(col_clusters))
  }
}

# Normalize consensus matrices
row_consensus <- row_consensus / num_iter
col_consensus <- col_consensus / num_iter

# Step 2: Compute PAC scores ===================================================
u1 <- 0.1
u2 <- 0.9

pac_row <- sum(row_consensus > u1 & row_consensus < u2) / length(row_consensus)
pac_col <- sum(col_consensus > u1 & col_consensus < u2) / length(col_consensus)

# Final PAC score (average of row and column PAC)
pac_score <- (pac_row + pac_col) / 2
print(paste("PAC Score for Biclustering:", pac_score))
