# ------------------------------------------------------------------------------
# Purpose: This script further aggregates categories so that no variable has
# more than four categories - help to reduce dimensions when dummy encoding. Ran
# after aggregation_master.R
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(plyr)
ukb_final <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final.rds')

# Distress score ===============================================================
table(ukb_final$distress_score)
str(ukb_final$distress_score)
ukb_final$distress_score <- revalue(ukb_final$distress_score, c("0" = "0", "1" = "1+", "2+" = "1+"))
table(ukb_final$distress_score)
str(ukb_final$distress_score)

# Employed =====================================================================
table(ukb_final$employment_status)
str(ukb_final$employment_status)
ukb_final$employment_status <- revalue(ukb_final$employment_status, c("None of the above" = "Other", 
                                                                      "In paid employment or self-employed" = "Employed", 
                                                                      "Retired" = "Retired",
                                                                      "Looking after home and/or family" = "Other",
                                                                      "Unable to work because of sickness or disability" = "Other",
                                                                      "Unemployed" = "Other",
                                                                      "Doing unpaid or voluntary work" = "Other",
                                                                      "Full or part-time student" = "Other"))
table(ukb_final$employment_status)
str(ukb_final$employment_status)

# Qualifications ===============================================================
table(ukb_final$qualifications)
str(ukb_final$qualifications)
ukb_final$qualifications <- revalue(ukb_final$qualifications, c("None of the above" = "Other", 
                                                                      "College or University degree" = "Degree", 
                                                                      "A levels/AS levels or equivalent" = "A/AS/O/GCSE level",
                                                                      "O levels/GCSEs or equivalent" = "A/AS/O/GCSE level",
                                                                      "CSEs or equivalent" = "Other",
                                                                      "NVQ or HND or HNC or equivalent" = "Other",
                                                                      "Other professional qualifications eg: nursing, teaching" = "Professional"))
table(ukb_final$qualifications)
str(ukb_final$qualifications)

# Able to confide ==============================================================
table(ukb_final$able_to_confide)
str(ukb_final$able_to_confide)
ukb_final$able_to_confide <- revalue(ukb_final$able_to_confide, c("Never or almost never" = "Never/Rarely", 
                                                                "Once every few months" = "Never/Rarely", 
                                                                "About once a month" = "Monthly/weekly",
                                                                "About once a week" = "Monthly/weekly",
                                                                "2-4 times a week" = "Monthly/weekly",
                                                                "Almost daily" = "Almost daily"))
table(ukb_final$able_to_confide)
str(ukb_final$able_to_confide)


# Alcohol intake ===============================================================
table(ukb_final$alcohol_intake)
str(ukb_final$alcohol_intake)
ukb_final$alcohol_intake <- revalue(ukb_final$alcohol_intake, c("Daily or almost daily" = "High", 
                                                                  "Three or four times a week" = "High", 
                                                                  "Once or twice a week" = "Medium",
                                                                  "One to three times a month" = "Medium",
                                                                  "Special occasions only" = "Never/Rarely",
                                                                  "Never" = "Never/Rarely"))
table(ukb_final$alcohol_intake)
str(ukb_final$alcohol_intake)

# Income =======================================================================
table(ukb_final$household_income)
str(ukb_final$household_income)
ukb_final$household_income <- revalue(ukb_final$household_income, c("Less than 18,000" = "Less than 18,000", 
                                                                "18,000 to 30,999" = "30,999 to 51,999", 
                                                                "31,000 to 51,999" = "30,999 to 51,999",
                                                                "52,000 to 100,000" = "Greater than 52,000",
                                                                "Greater than 100,000" = "Greater than 52,000"))
table(ukb_final$household_income)
str(ukb_final$household_income)

# Cars =========================================================================
table(ukb_final$vehicles_household)
str(ukb_final$vehicles_household)
ukb_final$vehicles_household <- revalue(ukb_final$vehicles_household, c("None" = "None",
                                                                        "One" = "One",
                                                                        "Two" = "Two or more",
                                                                        "Three" = "Two or more",
                                                                        "Four or more" = "Two or more"))
table(ukb_final$vehicles_household)
str(ukb_final$vehicles_household)

# Own rent =====================================================================
table(ukb_final$own_rent)
str(ukb_final$own_rent)
ukb_final$own_rent <- revalue(ukb_final$own_rent, c("None of the above" = "Other",
                                                    "Own outright (by you or someone in your household)" = "Own outright",
                                                    "Own with a mortgage" = "Own with a mortgage",
                                                    "Rent - from local authority, local council, housing association" = "Other",
                                                    "Rent - from private landlord or letting agency" = "Other",
                                                    "Pay part rent and part mortgage (shared ownership)" = "Other",
                                                    "Live in accommodation rent free" = "Other"))
table(ukb_final$own_rent)
str(ukb_final$own_rent)

# Num household ================================================================
ukb_final$num_household <- as.factor(ukb_final$num_household)
table(ukb_final$num_household)
str(ukb_final$num_household)
ukb_final$num_household <- revalue(ukb_final$num_household, c("Alone" = "Alone",
                                                    "Community home" = "Community home",
                                                    "Large" = "Large",
                                                    "Small" = "Small",
                                                    "1" = "Alone",
                                                    "3" = "Small",
                                                    "4" = "Large"))
table(ukb_final$num_household)
str(ukb_final$num_household)

ukb_final$multiple_deprivation_index <- as.double(ukb_final$multiple_deprivation_index)

# Save with new categories =====================================================
saveRDS(ukb_final, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds')
