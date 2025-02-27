rm(list = ls())
library(dplyr)
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')
ukb_recoded1 <- ukb_recoded

## Index of multiple deprivation =========================
# Make deciles for multiple deprivation for each region respectively, combine into one column
ukb_recoded1 <- mutate(ukb_recoded1, multiple_deprivation_e_deciles = ntile(multiple_deprivation_england.0.0, 10))
ukb_recoded1 <- mutate(ukb_recoded1, multiple_deprivation_s_deciles = ntile(multiple_deprivation_scotland.0.0, 10))
ukb_recoded1 <- mutate(ukb_recoded1, multiple_deprivation_w_deciles = ntile(multiple_deprivation_wales.0.0, 10))
ukb_recoded1 <- mutate(ukb_recoded1, multiple_deprivation_index = coalesce(multiple_deprivation_e_deciles,
                                                     multiple_deprivation_s_deciles,
                                                     multiple_deprivation_w_deciles))
ukb_recoded1 <- select(ukb_recoded1, -'multiple_deprivation_england.0.0', -'multiple_deprivation_scotland.0.0', -'multiple_deprivation_wales.0.0',
            -'multiple_deprivation_e_deciles', -'multiple_deprivation_s_deciles', -'multiple_deprivation_w_deciles')

## Current employment status =============================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$employment_status.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'employment_status' = 'employment_status.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, employment_status = na_if(ukb_recoded1$employment_status, 'Prefer not to answer'))
ukb_recoded1 <- select(ukb_recoded1, -'employment_status.0.1', -'employment_status.0.2', -'employment_status.0.3', 
            -'employment_status.0.4', -'employment_status.0.5', -'employment_status.0.6')


## Qualifications ========================================
# Didn't end up doing because university should take priority over professional qualifications (Ruben)
# If a row has 'Other professional qualifications eg: nursing, teaching' in any column, should appear in final column
# Otherwise, the value in qualifications.0.0 should appear in final column
# 'Prefer not to answer' & NA -> set to NA in final column
# check_prof <- function(row, col1_value) {
#   if (any(row == 'Other professional qualifications eg: nursing, teaching', na.rm=TRUE)) {
#     return('Other professional qualifications eg: nursing, teaching')
#   } else if (col1_value %in% c('Prefer not to answer', NA)) {
#     return(NA)
#   } else {
#     return(col1_value)
#   }
# }
# ukb_recoded1$qualifications <- apply(ukb_recoded1, 1, function(row) check_prof(row, row['qualifications.0.0']))

# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$qualifications.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'qualifications' = 'qualifications.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, qualifications = na_if(ukb_recoded1$qualifications, 'Prefer not to answer'))
ukb_recoded1 <- select(ukb_recoded1, -'qualifications.0.1', -'qualifications.0.2', 
                       -'qualifications.0.3', -'qualifications.0.4', -'qualifications.0.5')

## Average household income before tax ===================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$household_income.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'household_income' = 'household_income.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, household_income = na_if(ukb_recoded1$household_income, 'Prefer not to answer'))

## Own or rent accommodation lived in =====================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$own_rent.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'own_rent' = 'own_rent.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, own_rent = na_if(ukb_recoded1$own_rent, 'Prefer not to answer'))

## Gas or solid-fuel cooking/heating =====================
# Make binary columns gas_cooker, gas_fire, and gas_solid for each type of fuel
table(ukb_recoded1$gas_solid.0.0)
gas_cooker <- as.numeric(ukb_recoded1$gas_solid.0.0 == 'A gas hob or gas cooker')
ukb_recoded1$gas_cooker <- gas_cooker
ukb_recoded1$gas_cooker[is.na(ukb_recoded1$gas_cooker)] <- 0

gas_fire <- as.numeric(ukb_recoded1$gas_solid.0.0 == 'A gas fire that you use regularly in winter time' 
                       | ukb_recoded1$gas_solid.0.1 == 'A gas fire that you use regularly in winter time')
ukb_recoded1$gas_fire <- gas_fire
ukb_recoded1$gas_fire[is.na(ukb_recoded1$gas_fire)] <- 0

solid_fire <- as.numeric(ukb_recoded1$gas_solid.0.0 == 'An open solid fuel fire that you use regularly in winter time'
                         | ukb_recoded1$gas_solid.0.1 == 'An open solid fuel fire that you use regularly in winter time'
                         | ukb_recoded1$gas_solid.0.2 == 'An open solid fuel fire that you use regularly in winter time')
ukb_recoded1$solid_fire <- solid_fire
ukb_recoded1$solid_fire[is.na(ukb_recoded1$solid_fire)] <- 0

# If NA in gas_solid.0.0, set all the binary columns for that row to NA
ukb_recoded1 <- mutate(ukb_recoded1, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(is.na(gas_solid.0.0), NA, .)))

# If 'Do not know' or 'Prefer not to answer' in gas_solid.0.0, set all the binary columns for that row to NA
ukb_recoded1 <- mutate(ukb_recoded1, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(gas_solid.0.0 == 'Do not know', NA, .)))
ukb_recoded1 <- mutate(ukb_recoded1, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(gas_solid.0.0 == 'Prefer not to answer', NA, .)))

ukb_recoded1 <- ukb_recoded1 %>% select(-'gas_solid.0.0', -'gas_solid.0.1', -'gas_solid.0.2')

# Change gas_cooker, gas_fire, and solid_fire to factors instead of numeric
ukb_recoded1$gas_cooker <- as.factor(ukb_recoded1$gas_cooker)
ukb_recoded1$gas_fire <- as.factor(ukb_recoded1$gas_fire)
ukb_recoded1$solid_fire <- as.factor(ukb_recoded1$solid_fire)

## Number in household ===================================
# Set num_household from ukb_extracted to num_household in ukb_recoded - the recoding set everything to NAs
# Rename, if 'Prefer not to answer' set to NA
ukb_recoded1$num_household.0.0 <- ukb_extracted$num_household.0.0
ukb_recoded1 <- rename(ukb_recoded1, 'num_household' = 'num_household.0.0')
# Factoring into alone, small, and large
# Figuring out what these cut offs should be
summary(ukb_recoded1$num_household)
hist(ukb_recoded1$num_household)
ukb_recoded1 <- mutate(ukb_recoded1,
                       less_than_10 = ifelse(num_household <= 10, num_household, NA),
                       greater_than_10 = ifelse(num_household > 10, num_household, NA))
hist(ukb_recoded1$less_than_10)
hist(ukb_recoded1$greater_than_10)
# if less than 0 set to NA
# 1: alone
# 2-3: small
# 4-100: large
sum(ukb_recoded1$num_household < 0, na.rm = TRUE) # how many na values
ukb_recoded1$num_household <- ifelse(ukb_recoded1$num_household < 0, NA, ukb_recoded1$num_household)
ukb_recoded1$num_household <- ifelse(
  ukb_recoded1$num_household == 1, 'Alone',
  ifelse(ukb_recoded1$num_household %in% 2:3, 'Small',
         ifelse(ukb_recoded1$num_household %in% 4:6, 'Large',
                ifelse(ukb_recoded1$num_household >= 7, 'Community home', NA))))
table(ukb_recoded1$num_household)

## Number of vehicles ====================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$vehicles_household.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'vehicles_household' = 'vehicles_household.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, vehicles_household = na_if(ukb_recoded1$vehicles_household, 'Prefer not to answer'))







