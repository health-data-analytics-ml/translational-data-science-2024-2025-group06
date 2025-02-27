rm(list = ls())
ukb_recoded <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_recoded.rds')
ukb_extracted <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_extracted.rds')
ukb_recoded1 <- ukb_recoded
View(ukb_recoded1)

## Sex =========================================================================
# Rename
table(ukb_recoded1$sex.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'sex' = 'sex.0.0')

## Age =========================================================================
# Rename, drop year_of_birth
summary(ukb_recoded$age.0.0)
ukb_recoded <- rename(ukb_recoded, 'age' = 'age.0.0')
ukb_recoded <- select(ukb_recoded, -'year_of_birth.0.0')

## Ethnic background ===========================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$ethnic_background.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'ethnic_background' = 'ethnic_background.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, ethnic_background = na_if(ukb_recoded1$ethnic_background, 'Prefer not to answer'))

## Breastfed as a baby =========================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$breastfed.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'breastfed' = 'breastfed.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, breastfed = na_if(ukb_recoded1$breastfed, 'Prefer not to answer'))

## Maternal smoking ============================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$maternal_smoking.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'maternal_smoking' = 'maternal_smoking.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, maternal_smoking = na_if(ukb_recoded1$maternal_smoking, 'Prefer not to answer'))

## Able to confide =============================================================
# Rename, if 'Prefer not to answer' set to NA
table(ukb_recoded1$able_to_confide.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'able_to_confide' = 'able_to_confide.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, able_to_confide = na_if(ukb_recoded1$able_to_confide, 'Prefer not to answer'))

## Neuroticism Score ===========================================================
# Rename
summary(ukb_recoded1$neuro_score.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'neuro_score' = 'neuro_score.0.0')

## Anxiety =====================================================================
# Rename
summary(ukb_recoded1$anxiety.0.0)
ukb_recoded1 <- rename(ukb_recoded1, 'anxiety' = 'anxiety.0.0')
ukb_recoded1 <- mutate(ukb_recoded1, anxiety = na_if(ukb_recoded1$anxiety, 'Prefer not to answer'))

## Distress score ==============================================================
# Add up any number of responses of distress for each person to create distress score
ukb_recoded1$distress_experience.0.1 <- as.character(ukb_recoded1$distress_experience.0.1) 
ukb_recoded1$distress_experience.0.2 <- as.character(ukb_recoded1$distress_experience.0.2) 
ukb_recoded1$distress_experience.0.3 <- as.character(ukb_recoded1$distress_experience.0.3) 
ukb_recoded1$distress_experience.0.4 <- as.character(ukb_recoded1$distress_experience.0.4) 
ukb_recoded1$distress_experience.0.5 <- as.character(ukb_recoded1$distress_experience.0.5) 

combine_distress <- function(one_row) { 
  if ("None of above" %in% one_row) { 
    return(0) 
  } 
  if ("Prefer not to say" %in% one_row) {  
    return(NA)  
  } 
  return(sum(!is.na(one_row) & one_row != "")) 
} 
ukb_recoded1$distress_score <- apply( 
  ukb_recoded1[, c("distress_experience.0.1", "distress_experience.0.2", "distress_experience.0.3", "distress_experience.0.4", "distress_experience.0.5")], 
  1, 
  combine_distress 
) 



