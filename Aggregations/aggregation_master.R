# Read in data
rm(list = ls())
library(dplyr)
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')

# (a) Sociodemographic Variables ============================

## (i) Index of multiple deprivation =========================
# Make deciles for multiple deprivation for each region respectively, combine into one column
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_e_deciles = ntile(multiple_deprivation_england.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_s_deciles = ntile(multiple_deprivation_scotland.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_w_deciles = ntile(multiple_deprivation_wales.0.0, 10))
ukb_recoded <- mutate(ukb_recoded, multiple_deprivation_index = coalesce(multiple_deprivation_e_deciles,
                                                                           multiple_deprivation_s_deciles,
                                                                           multiple_deprivation_w_deciles))
ukb_recoded <- select(ukb_recoded, -'multiple_deprivation_england.0.0', -'multiple_deprivation_scotland.0.0', -'multiple_deprivation_wales.0.0',
                       -'multiple_deprivation_e_deciles', -'multiple_deprivation_s_deciles', -'multiple_deprivation_w_deciles')

## (ii) Current employment status =============================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$employment_status.0.6)
ukb_recoded <- rename(ukb_recoded, 'employment_status' = 'employment_status.0.0')
ukb_recoded <- mutate(ukb_recoded, employment_status = na_if(ukb_recoded$employment_status, 'Prefer not to answer'))
ukb_recoded <- select(ukb_recoded, -'employment_status.0.1', -'employment_status.0.2', -'employment_status.0.3', 
                       -'employment_status.0.4', -'employment_status.0.5', -'employment_status.0.6')
ukb_recoded$employment_status <- droplevels(ukb_recoded$employment_status)

## (iii) Qualifications ========================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$qualifications.0.0)
ukb_recoded <- rename(ukb_recoded, 'qualifications' = 'qualifications.0.0')
ukb_recoded <- mutate(ukb_recoded, qualifications = na_if(ukb_recoded$qualifications, 'Prefer not to answer'))
ukb_recoded <- select(ukb_recoded, -'qualifications.0.1', -'qualifications.0.2', 
                       -'qualifications.0.3', -'qualifications.0.4', -'qualifications.0.5')
ukb_recoded$qualifications <- droplevels(ukb_recoded$qualifications)

## (iv) Average household income before tax ===================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$household_income.0.0)
ukb_recoded <- rename(ukb_recoded, 'household_income' = 'household_income.0.0')
ukb_recoded <- mutate(ukb_recoded, household_income = na_if(ukb_recoded$household_income, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, household_income = na_if(ukb_recoded$household_income, 'Do not know'))
ukb_recoded$household_income <- droplevels(ukb_recoded$household_income)
table(ukb_recoded$household_income)

## (v) Own or rent accommodation lived in =====================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$own_rent.0.0)
ukb_recoded <- rename(ukb_recoded, 'own_rent' = 'own_rent.0.0')
ukb_recoded <- mutate(ukb_recoded, own_rent = na_if(ukb_recoded$own_rent, 'Prefer not to answer'))
ukb_recoded$own_rent <- droplevels(ukb_recoded$own_rent)
table(ukb_recoded$own_rent)

## (vi) Gas or solid-fuel cooking/heating =====================
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

## (vii) Number in household ===================================
# Set num_household from ukb_extracted to num_household in ukb_recoded - the recoding set everything to NAs
# Rename, if 'Prefer not to answer' set to NA
ukb_recoded$num_household.0.0 <- ukb_extracted$num_household.0.0
ukb_recoded <- rename(ukb_recoded, 'num_household' = 'num_household.0.0')
# Factoring into alone, small, large
# if less than 0 set to NA
# 1: alone
# 2-3: small
# 4-100: large
sum(ukb_recoded$num_household < 0, na.rm = TRUE) # how many na values
ukb_recoded$num_household <- ifelse(
  ukb_recoded$num_household == 1, 'Alone',
  ifelse(ukb_recoded$num_household %in% 2:3, 'Small',
         ifelse(ukb_recoded$num_household %in% 4:6, 'Large',
                ifelse(ukb_recoded$num_household >= 7, 'Community home', NA))))
table(ukb_recoded$num_household)

## (viii) Number of vehicles ====================================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$vehicles_household.0.0)
ukb_recoded <- rename(ukb_recoded, 'vehicles_household' = 'vehicles_household.0.0')
ukb_recoded <- mutate(ukb_recoded, vehicles_household = na_if(ukb_recoded$vehicles_household, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, vehicles_household = na_if(ukb_recoded$vehicles_household, 'Do not know'))
ukb_recoded$vehicles_household <- droplevels(ukb_recoded$vehicles_household)
table(ukb_recoded$vehicles_household)

# (b) Demographic, Childhood, Psychosocial Variables =================

## (i) Sex ============================================
# Rename
table(ukb_recoded$sex.0.0)
ukb_recoded <- rename(ukb_recoded, 'sex' = 'sex.0.0')

## (ii) Age ============================================
# Rename, drop year_of_birth
summary(ukb_recoded$age.0.0)
ukb_recoded <- rename(ukb_recoded, 'age' = 'age.0.0')
ukb_recoded <- select(ukb_recoded, -'year_of_birth.0.0')

## (iii) Ethnic background ==============================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$ethnic_background.0.0)
ukb_recoded <- rename(ukb_recoded, 'ethnic_background' = 'ethnic_background.0.0')
ukb_recoded <- mutate(ukb_recoded, ethnic_background = na_if(ukb_recoded$ethnic_background, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, ethnic_background = na_if(ukb_recoded$ethnic_background, 'Do not know'))
ukb_recoded$ethnic_background <- droplevels(ukb_recoded$ethnic_background)
table(ukb_recoded$ethnic_background)

## (iv) Breastfed as a baby ============================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$breastfed.0.0)
ukb_recoded <- rename(ukb_recoded, 'breastfed' = 'breastfed.0.0')
ukb_recoded <- mutate(ukb_recoded, breastfed = na_if(ukb_recoded$breastfed, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, breastfed = na_if(ukb_recoded$breastfed, 'Do not know'))
ukb_recoded$breastfed <- droplevels(ukb_recoded$breastfed)
table(ukb_recoded$breastfed)

## (v) Maternal smoking ===============================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$maternal_smoking.0.0)
ukb_recoded <- rename(ukb_recoded, 'maternal_smoking' = 'maternal_smoking.0.0')
ukb_recoded <- mutate(ukb_recoded, maternal_smoking = na_if(ukb_recoded$maternal_smoking, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, maternal_smoking = na_if(ukb_recoded$maternal_smoking, 'Do not know'))
ukb_recoded$maternal_smoking <- droplevels(ukb_recoded$maternal_smoking)
table(ukb_recoded$maternal_smoking)

## (vi) Childhood sunburn ==============================
ukb_recoded <- select(ukb_recoded, -'childhood_sunburn.0.0')

## (vii) Able to confide ================================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
table(ukb_recoded$able_to_confide.0.0)
ukb_recoded <- rename(ukb_recoded, 'able_to_confide' = 'able_to_confide.0.0')
ukb_recoded <- mutate(ukb_recoded, able_to_confide = na_if(ukb_recoded$able_to_confide, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, able_to_confide = na_if(ukb_recoded$able_to_confide, 'Do not know'))
ukb_recoded$able_to_confide <- droplevels(ukb_recoded$able_to_confide)
table(ukb_recoded$able_to_confide)

## (viii) Neuroticism Score ==============================
# Rename
summary(ukb_recoded$neuro_score.0.0)
ukb_recoded <- rename(ukb_recoded, 'neuro_score' = 'neuro_score.0.0')

## (ix) Anxiety =======================================
# Rename, if 'Prefer not to answer' or 'Do not know' set to NA
summary(ukb_recoded$anxiety.0.0)
ukb_recoded <- rename(ukb_recoded, 'anxiety' = 'anxiety.0.0')
ukb_recoded <- mutate(ukb_recoded, anxiety = na_if(ukb_recoded$anxiety, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, anxiety = na_if(ukb_recoded$anxiety, 'Do not know'))
ukb_recoded$anxiety <- droplevels(ukb_recoded$anxiety)
table(ukb_recoded$anxiety)

## (x) Distress score ==============================
# Add up any number of responses of distress for each person to create neuroticism score
ukb_recoded$distress_experience.0.1 <- as.character(ukb_recoded$distress_experience.0.1) 
ukb_recoded$distress_experience.0.2 <- as.character(ukb_recoded$distress_experience.0.2) 
ukb_recoded$distress_experience.0.3 <- as.character(ukb_recoded$distress_experience.0.3) 
ukb_recoded$distress_experience.0.4 <- as.character(ukb_recoded$distress_experience.0.4) 
ukb_recoded$distress_experience.0.5 <- as.character(ukb_recoded$distress_experience.0.5) 

combine_distress <- function(one_row) { 
  if ("None of above" %in% one_row) { 
    return(0) 
  } 
  if ("Prefer not to say" %in% one_row) {  
    return(NA)  
  } 
  return(sum(!is.na(one_row) & one_row != "")) 
} 
ukb_recoded$distress_score <- apply( 
  ukb_recoded[, c("distress_experience.0.1", "distress_experience.0.2", "distress_experience.0.3", "distress_experience.0.4", "distress_experience.0.5")], 
  1, 
  combine_distress 
) 

ukb_recoded <- ukb_recoded %>% select(-'distress_experience.0.1', -'distress_experience.0.2',
                                      -'distress_experience.0.3', -'distress_experience.0.4',
                                      -'distress_experience.0.5', -'distress_experience.0.0')

ukb_recoded$distress_score <- as.numeric(as.factor(ukb_recoded$distress_score))
ukb_recoded <- ukb_recoded %>% mutate(distress_score = ifelse((distress_score == 3 | distress_score == 4 | distress_score == 5 | distress_score == 6), "2+", as.factor(distress_score)))
ukb_recoded <- ukb_recoded %>% mutate(distress_score = ifelse((distress_score == 1), 0, distress_score))
ukb_recoded <- ukb_recoded %>% mutate(distress_score = ifelse((distress_score == 2), 1, distress_score))
ukb_recoded$distress_score <- as.factor(ukb_recoded$distress_score)
table(ukb_recoded$distress_score)

# (c) Behavioural===================================================================

## (i) Sleep score =================================================================
# Using relevant columns from ukb_recoded and ukb_extracted
data_recoded <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds")
data_con <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds")

# Extract columns and change values to be 0 or 1 
# nap:  never/rarely(1) = 1
#       sometimes(2) = 1
#       usually(3) = 0
# insomnia:   never/rarely(1) = 1
#             sometimes(2) = 0
#             usually(3) = 0
# hrs_sleep: 7-9 = 1
#            other = 0
#            -1, -3 = "NA"

sleep_data <- data.frame(data_recoded$nap.0.0, data_recoded$insomnia.0.0, data_con$hrs_sleep.0.0)
colnames(sleep_data) <- c("nap.0.0", "insomnia.0.0", "hrs_sleep.0.0")

# Create the NAs
sleep_data[sleep_data == "Prefer not to answer"] <- NA
sleep_data$hrs_sleep.0.0[sleep_data$hrs_sleep.0.0 == -3] <- NA
sleep_data$hrs_sleep.0.0[sleep_data$hrs_sleep.0.0 == -1] <- NA

# Create the sleep score
sleep_data$hrs_sleep.0.0 <- as.numeric(sleep_data$hrs_sleep.0.0)
sleep_data <- sleep_data %>% mutate(nap_score = ifelse((nap.0.0 == "Never/rarely" | nap.0.0 == "Sometimes"), 1, 0),
                                    insomnia_score = ifelse(insomnia.0.0 == "Never/rarely", 1, 0),
                                    hrs_sleep_score = ifelse((hrs_sleep.0.0 > 6 & hrs_sleep.0.0 < 10), 1, 0))

sleep_data <- sleep_data %>% mutate(sleep_score = rowSums(sleep_data[,c("nap_score", "insomnia_score", "hrs_sleep_score")], na.rm=FALSE))

# Change the ukb_recoded dataset
ukb_recoded <- data.frame(ukb_recoded, sleep_data$sleep_score)
ukb_recoded <- rename(ukb_recoded, 'sleep_data' = 'sleep_data.sleep_score')
ukb_recoded <- ukb_recoded %>% select(-'nap.0.0', -'insomnia.0.0', -'hrs_sleep.0.0')
rm(data_recoded, data_con, sleep_data)


## (ii) Pack years ==================================================================
# If Never smoker, the NA is changed to 0
# nrow(ukb_recoded %>% filter(smoking_status.0.0 == "Never" & !is.na(pack_years.0.0)))
ukb_recoded <- ukb_recoded %>% mutate(pack_years.0.0 = ifelse((smoking_status.0.0 == "Never"), 0, pack_years.0.0))
ukb_recoded <- rename(ukb_recoded, 'pack_years' = 'pack_years.0.0')

## (iii) Smoking status ==============================================================
ukb_recoded$smoking_status.0.0[ukb_recoded$smoking_status.0.0 == "Prefer not to answer"] <- NA
ukb_recoded$smoking_status.0.0 <- droplevels(ukb_recoded$smoking_status.0.0)
ukb_recoded <- rename(ukb_recoded, 'smoking_status' = 'smoking_status.0.0')
table(ukb_recoded$smoking_status)
# nrow(ukb_recoded %>% filter(is.na(smoking_status) & !is.na(pack_years)))
nrow(ukb_recoded %>% filter(!is.na(smoking_status) & is.na(pack_years)))

## (iv) Alcohol_10_yrs ==============================================================
# If Never drank, the NA is changed to 0
ukb_recoded <- ukb_recoded %>% mutate(alcohol_10_yrs.0.0 = ifelse((alcohol_intake.0.0 == "Never"), 0, as.character(alcohol_10_yrs.0.0)))
ukb_recoded <- rename(ukb_recoded, 'alcohol_10_yrs' = 'alcohol_10_yrs.0.0')
# If 'Prefer not to answer' or 'Do not know' set to NA
ukb_recoded <- mutate(ukb_recoded, alcohol_10_yrs = na_if(ukb_recoded$alcohol_10_yrs, 'Prefer not to answer'))
ukb_recoded <- mutate(ukb_recoded, alcohol_10_yrs = na_if(ukb_recoded$alcohol_10_yrs, 'Do not know'))
ukb_recoded$alcohol_10_yrs <- as.factor(ukb_recoded$alcohol_10_yrs)
ukb_recoded$alcohol_10_yrs <- droplevels(ukb_recoded$alcohol_10_yrs)
table(ukb_recoded$alcohol_10_yrs)

## (v) Alcohol status ==============================================================
ukb_recoded$alcohol_intake.0.0[ukb_recoded$alcohol_intake.0.0 == "Prefer not to answer"] <- NA
ukb_recoded$alcohol_intake.0.0 <- droplevels(ukb_recoded$alcohol_intake.0.0)
ukb_recoded <- rename(ukb_recoded, 'alcohol_intake' = 'alcohol_intake.0.0')
table(ukb_recoded$alcohol_intake)

# (d) Environmental ===============================================================

## (i) No2_2010 2010 ==============================================================
summary(ukb_recoded$no2_2010.0.0)
ukb_recoded <- rename(ukb_recoded, 'no2_2010' = 'no2_2010.0.0')

## (ii) Pm10 2010 ==================================================================
summary(ukb_recoded$pm10.0.0)
ukb_recoded <- rename(ukb_recoded, 'pm10' = 'pm10.0.0')

## (iii) Pm2.5.0.0 2010 =============================================================
summary(ukb_recoded$pm2.5.0.0)
ukb_recoded <- rename(ukb_recoded, 'pm2.5' = 'pm2.5.0.0')

##  (iv) Traffic intensity ==========================================================
summary(ukb_recoded$traffic_intensity.0.0)
ukb_recoded <- rename(ukb_recoded, 'traffic_intensity' = 'traffic_intensity.0.0')

## (v) Inverse distance to major road =============================================
summary(ukb_recoded$inv_dis_maj_road.0.0)
ukb_recoded <- rename(ukb_recoded, 'inv_dis_maj_road' = 'inv_dis_maj_road.0.0')

## (vi) Greenspace percentage, buffer 1000m ========================================
summary(ukb_recoded$greenspace_1000m.0.0)
ukb_recoded <- rename(ukb_recoded, 'greenspace_1000m' = 'greenspace_1000m.0.0')

## (vii) Water percentage, buffer 1000m =============================================
summary(ukb_recoded$water_1000m.0.0)
ukb_recoded <- rename(ukb_recoded, 'water_1000m' = 'water_1000m.0.0')

## (viii) Distance (Euclidean) to coast ==============================================
summary(ukb_recoded$coast_distance.0.0)
ukb_recoded <- rename(ukb_recoded, 'coast_distance' = 'coast_distance.0.0')

# Merge diet and exercise scores
diet <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/Scores/diet.rds')
physical_activity <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/Scores/physical_activity.rds')
ukb_recoded <- merge(ukb_recoded, diet['diet_score'], by = 'row.names', all.x=TRUE)
row.names(ukb_recoded) <- ukb_recoded$Row.names
ukb_recoded$Row.names <- NULL
ukb_recoded <- merge(ukb_recoded, physical_activity['met_score'], by = 'row.names', all.x=TRUE)
row.names(ukb_recoded) <- ukb_recoded$Row.names
ukb_recoded$Row.names <- NULL

# Save full dataset as a .rds object =========================================
saveRDS(ukb_recoded, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_clean.rds')

# Save complete cases as a .rds object =======================================
complete_cases <- na.omit(ukb_recoded)
saveRDS(complete_cases, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_complete_cases.rds')

