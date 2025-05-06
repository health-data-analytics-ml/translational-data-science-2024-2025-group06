# ------------------------------------------------------------------------------
# Purpose: This script outlines the cleaning and recoding only for socioeconomic
# variables in the UK Biobank
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(dplyr)
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')

## Index of multiple deprivation ===============================================
# Make deciles for multiple deprivation for each region respectively, combine into one column
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_e_deciles = ntile(multiple_deprivation_england.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_s_deciles = ntile(multiple_deprivation_scotland.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_w_deciles = ntile(multiple_deprivation_wales.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_index = coalesce(multiple_deprivation_e_deciles,
                                                     multiple_deprivation_s_deciles,
                                                     multiple_deprivation_w_deciles))
ukb_recoded <- select(ukb_recoded, -'multiple_deprivation_england.0.0', -'multiple_deprivation_scotland.0.0', -'multiple_deprivation_wales.0.0',
            -'multiple_deprivation_e_deciles', -'multiple_deprivation_s_deciles', -'multiple_deprivation_w_deciles')

## Current employment status ===================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$employment_status.0.0)
ukb_recoded <- rename(ukb_recoded, 'employment_status' = 'employment_status.0.0')
ukb_recoded <- mutate(ukb_recoded, employment_status = na_if(ukb_recoded$employment_status, 'Prefer not to answer'))
ukb_recoded <- select(ukb_recoded, -'employment_status.0.1', -'employment_status.0.2', -'employment_status.0.3', 
            -'employment_status.0.4', -'employment_status.0.5', -'employment_status.0.6')


## Qualifications ==============================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$qualifications.0.0)
ukb_recoded <- rename(ukb_recoded, 'qualifications' = 'qualifications.0.0')
ukb_recoded <- mutate(ukb_recoded, qualifications = na_if(ukb_recoded$qualifications, 'Prefer not to answer'))
ukb_recoded <- select(ukb_recoded, -'qualifications.0.1', -'qualifications.0.2', 
                       -'qualifications.0.3', -'qualifications.0.4', -'qualifications.0.5')

## Average household income before tax =========================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$household_income.0.0)
ukb_recoded <- rename(ukb_recoded, 'household_income' = 'household_income.0.0')
ukb_recoded <- mutate(ukb_recoded, household_income = na_if(ukb_recoded$household_income, 'Prefer not to answer'))

## Own or rent accommodation lived in ==========================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$own_rent.0.0)
ukb_recoded <- rename(ukb_recoded, 'own_rent' = 'own_rent.0.0')
ukb_recoded <- mutate(ukb_recoded, own_rent = na_if(ukb_recoded$own_rent, 'Prefer not to answer'))

## Gas or solid-fuel cooking/heating ===========================================
# Make binary columns gas_cooker, gas_fire, and gas_solid for each type of fuel
table(ukb_recoded$gas_solid.0.0)
gas_cooker <- as.numeric(ukb_recoded$gas_solid.0.0 == 'A gas hob or gas cooker')
ukb_recoded$gas_cooker <- gas_cooker
ukb_recoded$gas_cooker[is.na(ukb_recoded$gas_cooker)] <- 0

gas_fire <- as.numeric(ukb_recoded$gas_solid.0.0 == 'A gas fire that you use regularly in winter time' 
                       | ukb_recoded$gas_solid.0.1 == 'A gas fire that you use regularly in winter time')
ukb_recoded$gas_fire <- gas_fire
ukb_recoded$gas_fire[is.na(ukb_recoded$gas_fire)] <- 0

solid_fire <- as.numeric(ukb_recoded$gas_solid.0.0 == 'An open solid fuel fire that you use regularly in winter time'
                         | ukb_recoded$gas_solid.0.1 == 'An open solid fuel fire that you use regularly in winter time'
                         | ukb_recoded$gas_solid.0.2 == 'An open solid fuel fire that you use regularly in winter time')
ukb_recoded$solid_fire <- solid_fire
ukb_recoded$solid_fire[is.na(ukb_recoded$solid_fire)] <- 0

# If NA in gas_solid.0.0, set all the binary columns for that row to NA
ukb_recoded <- mutate(ukb_recoded, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(is.na(gas_solid.0.0), NA, .)))

# If 'Do not know' or 'Prefer not to answer' in gas_solid.0.0, set all the binary columns for that row to NA
ukb_recoded <- mutate(ukb_recoded, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(gas_solid.0.0 == 'Do not know', NA, .)))
ukb_recoded <- mutate(ukb_recoded, across(c('gas_cooker', 'gas_fire', 'solid_fire'), ~ ifelse(gas_solid.0.0 == 'Prefer not to answer', NA, .)))

ukb_recoded <- ukb_recoded %>% select(-'gas_solid.0.0', -'gas_solid.0.1', -'gas_solid.0.2')

# Change gas_cooker, gas_fire, and solid_fire to factors instead of numeric
ukb_recoded$gas_cooker <- as.factor(ukb_recoded$gas_cooker)
ukb_recoded$gas_fire <- as.factor(ukb_recoded$gas_fire)
ukb_recoded$solid_fire <- as.factor(ukb_recoded$solid_fire)

## Number in household =========================================================
# Set num_household from ukb_extracted to num_household in ukb_recoded - the recoding set everything to NAs
# Rename, if 'Prefer not to answer' set to NA
ukb_recoded$num_household.0.0 <- ukb_extracted$num_household.0.0
ukb_recoded <- rename(ukb_recoded, 'num_household' = 'num_household.0.0')
# Factoring into alone, small, and large
# Figuring out what these cut offs should be
summary(ukb_recoded$num_household)
hist(ukb_recoded$num_household)
ukb_recoded <- mutate(ukb_recoded,
                       less_than_10 = ifelse(num_household <= 10, num_household, NA),
                       greater_than_10 = ifelse(num_household > 10, num_household, NA))
hist(ukb_recoded$less_than_10)
hist(ukb_recoded$greater_than_10)
# if less than 0 set to NA
# 1: alone
# 2-3: small
# 4-100: large
sum(ukb_recoded$num_household < 0, na.rm = TRUE) # how many na values
ukb_recoded$num_household <- ifelse(ukb_recoded$num_household < 0, NA, ukb_recoded$num_household)
ukb_recoded$num_household <- ifelse(
  ukb_recoded$num_household == 1, 'Alone',
  ifelse(ukb_recoded$num_household %in% 2:3, 'Small',
         ifelse(ukb_recoded$num_household %in% 4:6, 'Large',
                ifelse(ukb_recoded$num_household >= 7, 'Community home', NA))))
table(ukb_recoded$num_household)

## Number of vehicles ==========================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$vehicles_household.0.0)
ukb_recoded <- rename(ukb_recoded, 'vehicles_household' = 'vehicles_household.0.0')
ukb_recoded <- mutate(ukb_recoded, vehicles_household = na_if(ukb_recoded$vehicles_household, 'Prefer not to answer'))







