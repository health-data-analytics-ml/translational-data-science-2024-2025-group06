suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(missRanger))

data <- readRDS('/rds/general/user/hc724/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_outliers_removed.rds')

#data <- data %>% mutate(num_household = as.factor(num_household))

#toy_data <- data[1:1000, ]


data_imp <- missRanger(data)
# final_file <- data_imp$ximp
saveRDS(final_file, '/rds/general/user/hc724/projects/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_imputed_mr.rds')