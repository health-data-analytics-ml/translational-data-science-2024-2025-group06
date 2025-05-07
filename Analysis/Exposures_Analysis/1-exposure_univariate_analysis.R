# ------------------------------------------------------------------------------
# Purpose: This script runs univariate analysis for exposures - one logistic
# regression model per exposure per cluster (217 models total) adjusted for age,
# sex, and ethnic background. Generates datasets to use in analysis_plots 
# ------------------------------------------------------------------------------

# Data and libraries ===========================================================
rm(list = ls())
library(tidyverse)
library(ggrepel)
library(ggplot2)
gmm_model <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds')
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
ukb_analysis <- data.frame(cluster = gmm_model$classification, ukb_final_reduced)
ukb_analysis <- ukb_analysis %>% mutate(ethnic_background = ifelse((ethnic_background == "British" | 
                                                                      ethnic_background == "Black or Black British" |
                                                                      ethnic_background == "Asian or Asian British"), "British_ethnic_backgrounds", "Other_identified_ethnic_backgrounds"))
ukb_analysis$ethnic_background <- as.factor(ukb_analysis$ethnic_background)

str(ukb_analysis)
ukb_analysis$cluster <- as.factor(ukb_analysis$cluster)
ukb_analysis <- ukb_analysis %>% mutate(across(where(~is.double(.)), ~ as.numeric(scale(.))))


# Create identity columns for each cluster
ukb_analysis$cluster <- as.numeric(ukb_analysis$cluster)
ukb_analysis$ids <- row.names(ukb_analysis)
ukb_analysis <- dummy_cols(ukb_analysis, select_columns = 'cluster')

# Add the rownames back in, this is removed by dummy_cols
rownames(ukb_analysis) <- ukb_analysis$ids
ukb_analysis <- ukb_analysis[,-36]

factor_cols <- c("own_rent", "num_household", "vehicles_household", 
                 "household_income", "alcohol_intake", "alcohol_10_yrs", 
                 "breastfed", "maternal_smoking", "anxiety", "able_to_confide", 
                 "qualifications", "employment_status", "smoking_status",
                 "gas_cooker", "gas_fire", "solid_fire", "distress_score")

numeric_cols <- c("neuro_score", "pack_years", "no2_2010", "pm10", "pm2.5",
                  "traffic_intensity", "inv_dis_maj_road", "greenspace_1000m",
                  "water_1000m", "coast_distance", 
                  "multiple_deprivation_index", "sleep_data", "diet_score", "met_score")

# NUMERIC - build an empty matrix to add p-values from output of each univariate analysis
m <- matrix(data = NA, ncol = 14, nrow = 7)
colnames(m) <- numeric_cols

# NUMERIC - extracts p-value from each univariate logistic regression and saves in m
for(i in 1:7){
  # Select cluster i
  outcome <- ukb_analysis[,i+35]
  for(j in 1:14){
    # Select exposure j
    exposure <- numeric_cols[j]
    # Fit a logistic regression model
    model1 <- glm(outcome ~ ukb_analysis[,exposure] + age + sex + ethnic_background, data = ukb_analysis, family = 'binomial')
    # There's only one exposure variable as numeric 
    m[i,j] <- coef(summary(model1))[2,4]
  }
}

# FACTOR - selects all the relevant p-values from the regression
m2 <- m
for(j in 1:17){
  # Select exposure j
  exposure <- factor_cols[j]
  # There p-1 outputs for each categories, make a matrix to save the p-values for each cluster
  names_exp <- levels(ukb_analysis[,exposure])[-1]
  m_exp <- matrix(data = NA, ncol = length(names_exp), nrow = 7)
  full_names <- paste(names_exp, exposure, sep="")
  colnames(m_exp) <- full_names
  for(i in 1:7){
    # Fits the model with exposure j for all clusters
    # Select cluster j
    outcome <- ukb_analysis[,i+35]
    model1 <- glm(outcome ~ ukb_analysis[,exposure] + age + sex + ethnic_background, data = ukb_analysis, family = 'binomial')
    # Extracts p-values from the summary output with the exposure name in the title
    rows <- grep("ukb_analysis", rownames(coef(summary(model1))), value = TRUE)
    # Fill m_exp with the p-values for each category
    for(k in 1:length(rows)){
      m_exp[i,k] <- coef(summary(model1))[rows[k],4]
    }
  }
  # Add this to m2 and loop round, choosing a new exposure
  m2 <- cbind(m2, m_exp)
}

saveRDS(m2, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/univariate_pvalues.rds")

# Dataframe for Manhattan plot =================================================
# Assign each variable to a subgroup (for the plot)
Subgroups <- c("Psychosocial", "Behavioural", rep("Environmental", 8), 
               "Socio-economic", rep("Behavioural", 3), rep("Socio-economic", 9), 
               rep("Behavioural", 5), rep("Childhood", 2), rep("Psychosocial", 3), 
               rep("Socio-economic", 5), rep("Behavioural", 2), 
               rep("Socio-economic", 3), "Psychosocial")

# Take the negative p-value (for the plot)
manhattan_table <- data.frame(Variable = rep(colnames(m2), 7),
                        Subgroup = rep(Subgroups, 7),
                        Cluster = rep(1:7, each = 44),
                        pval = -as.vector(t(m2)))

saveRDS(manhattan_table, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/manhattan_table.rds")

# Dataframe for log-Manhattan plot =============================================
univariate_log <- -log(m2, base = 10)
univariate_log[univariate_log == Inf] <- 50
univariate_log[univariate_log >= 50] <- 50
saveRDS(univariate_log, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/univariate_log.rds")

manhattan_log <- data.frame(Variable = rep(colnames(univariate_log), 7),
                      Subgroup = rep(Subgroups, 7),
                      Cluster = rep(1:7, each = 44),
                      pval = as.vector(t(univariate_log)))

saveRDS(manhattan_log, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/manhattan_log.rds")

figure_names <- data.frame(Variable = c(sort(unique(manhattan_log$Variable)), "smoking_status_Previous",
                                        "vehicles_household_Two or more", "anxiety_Yes", "age",
                                        "ethnic_backgroundOther_identified_ethnic_backgrounds", "sexMale",
                                        "smoking_statusCurrent", "smoking_statusPrevious",
                                        "vehicles_householdOne", "vehicles_householdTwo or more", "anxietyYes",
                                        "smoking_status_Never", "num_household_Small", "solid_fire_1",
                                        "vehicles_household_None", "num_householdCommunity home", "num_householdLarge",
                                        "num_householdSmall", "solid_fire1"),
                           figure_variable = c("Distress score (1+)",
                                               "Gas cooker", "Gas fire", "Solid fire", "Income £30-51,999",
                                               "A/AS/O/GCSE", "Alcohol 10yrs ago (same)", "Confide (almost daily)",
                                               "Coast distance", "Num. household (community home)",
                                               "Smoking (current)", "Degree", "Diet score", "Employed",
                                               "Income >£52,000", "Greenspace 1000m", "Inv. distance major road",
                                               "Num. household (large)", "Alcohol 10yrs ago (less now)", 
                                               "Alcohol (medium)", "MET score", "Confide (monthly/weekly)", 
                                               "Alcohol 10yrs ago (more now)", "IMD", "Neuroticism score",
                                               "Alcohol (never/rarely)", "NO2 (2010)", "Vehicles (one)",
                                               "Own house", "Own house (mortgage)", "Pack years", "PM10", "PM2.5",
                                               "Smoking (previous)", "Professional qual.", "Retired",
                                               "Sleep score", "Num. household (small)", "Traffic intensity",
                                               "Vehicles (2+)", "Water 1000m", "Anxiety", "Breastfed", "Maternal smoking",
                                               "Smoking (previous)", "Vehicles (2+)", "Anxiety", "Age",
                                               "Ethnic background (Other)", "Sex (Male)", "Smoking (current)", "Smoking (previous)",
                                               "Vehicles (1)", "Vehicles (2+)", "Anxiety", "Smoking (never)", "Num. household (small)",
                                               "Solid fire", "Vehicles (none)", "Num. household (community)", "Num household (large)",
                                               "Num. household (small)", "Solid fire"))

saveRDS(figure_names, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/figure_names.rds")
