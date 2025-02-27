suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missForest))

data <- readRDS('/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_missing_removed.rds')

data <- data %>% mutate(num_household = as.factor(num_household))

#toy_data <- data[1:1000, ]


data_imp <- missForest(data)
final_file <- data_imp$ximp
saveRDS(final_file, '/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed_rf.rds')
