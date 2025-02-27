#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N dict

library(tidyverse)
library(VIM)

data <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')
#data_small <- data[1:50, ]

data_imp <- kNN(data, k = 10, imp_var = FALSE)

saveRDS(data_imp, file = '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')



