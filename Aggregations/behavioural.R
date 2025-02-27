## Sleep score =================================================================
# hrs_sleep: 888 (original NA) + 3326 NAs created from {-1,-3}
# nap : 892 NA
# insomnia: 892 NA

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

#sleep_data$nap.0.0[sleep_data$nap.0.0 == "Prefer not to answer"] <- NA
#sleep_data$insomnia.0.0[sleep_data$insomnia.0.0 == "Prefer not to answer"] <- NA

sleep_data[sleep_data == "Prefer not to answer"] <- NA

sleep_data$hrs_sleep.0.0[sleep_data$hrs_sleep.0.0 == -3] <- NA
sleep_data$hrs_sleep.0.0[sleep_data$hrs_sleep.0.0 == -1] <- NA
sleep_data$hrs_sleep.0.0 <- as.numeric(sleep_data$hrs_sleep.0.0)

sleep_data <- sleep_data %>% mutate(nap_score = ifelse((nap.0.0 == "Never/rarely" | nap.0.0 == "Sometimes"), 1, 0),
                                    insomnia_score = ifelse(insomnia.0.0 == "Never/rarely", 1, 0),
                                    hrs_sleep_score = ifelse((hrs_sleep.0.0 > 6 & hrs_sleep.0.0 < 10), 1, 0))

sleep_data <- sleep_data %>% mutate(sleep_score = 
                                      rowSums(sleep_data[,c("nap_score", "insomnia_score", "hrs_sleep_score")], 
                                              na.rm=FALSE))
rm(data_recoded, data_con)
ukb_recoded1 <- data.frame(ukb_recoded1, sleep_data$sleep_score)

# Pack_years ===================================================================
# If Never smoker, the NA is changed to 0
ukb_recoded1 <- ukb_recoded1 %>% mutate(pack_years.0.0 = ifelse((smoking_status.0.0 == "Never"), 0, pack_years.0.0))

# Smoking status ===============================================================
ukb_recoded1$smoking_status.0.0[ukb_recoded1$smoking_status.0.0 == "Prefer not to answer"] <- NA
ukb_recoded1$smoking_status.0.0 <- droplevels(ukb_recoded1$smoking_status.0.0)

# Alcohol_10_yrs ===============================================================
# If Never drank, the NA is changed to 0
ukb_recoded1 <- ukb_recoded1 %>% mutate(alcohol_10_yrs.0.0 = ifelse((alcohol_intake.0.0 == "Never"), 0, alcohol_10_yrs.0.0))

# Alcohol status ===============================================================
ukb_recoded1$alcohol_intake.0.0[ukb_recoded1$alcohol_intake.0.0 == "Prefer not to answer"] <- NA
ukb_recoded1$alcohol_intake.0.0 <- droplevels(ukb_recoded1$alcohol_intake.0.0)

# 351358 NA values
# Status can be Current, Previous, Never, NA
# smoking <- data.frame(ukb_recoded$smoking_status.0.0, ukb_recoded$pack_years.0.0)
# colnames(smoking) <- c("smoking_status.0.0", "pack_years.0.0")
# # If Never smoker, the NA is changed to 0
# smoking <- smoking %>% mutate(pack_years.0.0 = ifelse((smoking_status.0.0 == "Never"), 0, pack_years.0.0))


