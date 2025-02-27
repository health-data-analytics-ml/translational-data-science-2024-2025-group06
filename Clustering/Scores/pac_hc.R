library(mclust)
library(diceR)
set.seed(8)
data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds')


# PAC Score for Hierarchical ----------------------------------------------

consensus_hc <- consensus_cluster(data, nk = 10, algorithm = 'hc')
gap_pac <- PAC(consensus_hc)
print(gap_pac)