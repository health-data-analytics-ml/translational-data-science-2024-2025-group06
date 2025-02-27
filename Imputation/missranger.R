suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missRanger))

data <- readRDS('/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

#data <- data %>% mutate(num_household = as.factor(num_household))

#toy_data <- data[1:1000, ]


data_imp <- missRanger(data, seed = 8, maxiter = 3, num.trees = 100, pmm.k = 5)
saveRDS(data_imp, '/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed.rds')