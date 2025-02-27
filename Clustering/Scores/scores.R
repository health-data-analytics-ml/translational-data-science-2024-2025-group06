
library(diceR)
library(cluster)
library(mclust)

data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')
toy_data <- data[1:5000, ]
#gap_h <- clusGap(data, FUNcluster = hcut, k.max = , B =50)

# GMM Scores --------------------------------------------------------------

# Gap score for GMM
#gap_gmm <- clusGap(toy_data, FUNcluster = 'Mclust', K.max = 10, B = 50)
#print(gap_gmm)

# Pac Score for GMM
consensus_gmm <- consensus_cluster(data, nk = 10, algorithm = 'gmm')
gap_pac <- PAC(consensus_gmm)
print(gap_pac)
