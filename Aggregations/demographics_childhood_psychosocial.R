# ------------------------------------------------------------------------------
# Purpose: This script outlines the cleaning and recoding only for demographic,
# childhood, and psychosocial variables in the UK Biobank
# ------------------------------------------------------------------------------

# Data =========================================================================
rm(list = ls())
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')

## Sex =========================================================================
# Rename
table(ukb_recoded$sex.0.0)
ukb_recoded <- rename(ukb_recoded, 'sex' = 'sex.0.0')

## Age =========================================================================
# Rename, drop year_of_birth
summary(ukb_recoded$age.0.0)
ukb_recoded <- rename(ukb_recoded, 'age' = 'age.0.0')
ukb_recoded <- select(ukb_recoded, -'year_of_birth.0.0')

## Ethnic background ===========================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$ethnic_background.0.0)
ukb_recoded <- rename(ukb_recoded, 'ethnic_background' = 'ethnic_background.0.0')
ukb_recoded <- mutate(ukb_recoded, ethnic_background = na_if(ukb_recoded$ethnic_background, 'Prefer not to answer'))

## Breastfed as a baby =========================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$breastfed.0.0)
ukb_recoded <- rename(ukb_recoded, 'breastfed' = 'breastfed.0.0')
ukb_recoded <- mutate(ukb_recoded, breastfed = na_if(ukb_recoded$breastfed, 'Prefer not to answer'))

## Maternal smoking ============================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$maternal_smoking.0.0)
ukb_recoded <- rename(ukb_recoded, 'maternal_smoking' = 'maternal_smoking.0.0')
ukb_recoded <- mutate(ukb_recoded, maternal_smoking = na_if(ukb_recoded$maternal_smoking, 'Prefer not to answer'))

## Able to confide =============================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded$able_to_confide.0.0)
ukb_recoded <- rename(ukb_recoded, 'able_to_confide' = 'able_to_confide.0.0')
ukb_recoded <- mutate(ukb_recoded, able_to_confide = na_if(ukb_recoded$able_to_confide, 'Prefer not to answer'))

## Neuroticism Score ===========================================================
# Rename
summary(ukb_recoded$neuro_score.0.0)
ukb_recoded <- rename(ukb_recoded, 'neuro_score' = 'neuro_score.0.0')

## Anxiety =====================================================================
# Rename
summary(ukb_recoded$anxiety.0.0)
ukb_recoded <- rename(ukb_recoded, 'anxiety' = 'anxiety.0.0')
ukb_recoded <- mutate(ukb_recoded, anxiety = na_if(ukb_recoded$anxiety, 'Prefer not to answer'))

## Distress score ==============================================================
# Add up any number of responses of distress for each person to create distress score
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