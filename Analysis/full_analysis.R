# Overall analysis 
rm(list = ls())

# Library ==============
library(fastDummies)
library(qqman)
library(tidyverse)
library(ggrepel)

# Full dataset ============
# Make dataset with clusters, demographics, one-hot encoded data 
gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
ukb_final_reduced <- ukb_final_reduced %>% mutate(across(where(~is.double(.)), ~ as.numeric(scale(.))))

ukb_analysis <- data.frame(cluster = gmm_model$classification, ukb_final_reduced)
ukb_analysis <- ukb_analysis %>% mutate(ethnic_background = ifelse((ethnic_background == "British" | 
                                                              ethnic_background == "Black or Black British" |
                                                              ethnic_background == "Asian or Asian British"), "British_ethnicity", "Other_ethnicity"))

ukb_analysis$ethnic_background <- as.factor(ukb_analysis$ethnic_background)
ukb_analysis <- dummy_cols(ukb_analysis, select_columns = 'cluster')
saveRDS(ukb_analysis, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")

# Split data =============
set.seed(1342)
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
train <- sample(1:nrow(ukb_analysis), floor(0.8 * nrow(ukb_analysis)))
test <- seq(1, nrow(ukb_analysis))[-train] # gets the 20% test set, 8361
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set, 20901
train <- setdiff(train, select) # gets the 30% training set, 12542

# Univariate analysis ===============
# 100% of data used
factor_cols <- c("own_rent", "num_household", "vehicles_household", 
                  "household_income", "alcohol_intake", "alcohol_10_yrs", 
                  "breastfed", "maternal_smoking", "anxiety", "able_to_confide", 
                  "qualifications", "employment_status", "smoking_status",
                  "gas_cooker", "gas_fire", "solid_fire", "distress_score")
numeric_cols <- c("neuro_score", "pack_years", "no2_2010", "pm10", "pm2.5",
                  "traffic_intensity", "inv_dis_maj_road", "greenspace_1000m",
                  "water_1000m", "coast_distance", 
                  "multiple_deprivation_index", "sleep_data", "diet_score", "met_score")

# NUMERIC Builds an empty matrix to add p-values from output of each univariate analysis
m <- matrix(data = NA, ncol = 14, nrow = 8)
colnames(m) <- numeric_cols

# NUMERIC Extracts p-value from each univariate logistic regression and saves in m
for(i in 1:8){
  outcome <- ukb_analysis[,i+35]
  for(j in 1:14){
    exposure <- numeric_cols[i]
    model1 <- glm(outcome ~ ukb_analysis[,exposure] + age + sex + ethnic_background, data = ukb_analysis, family = 'binomial')
    m[i,j] <- coef(summary(model1))[2,4]
  }
}

m2 <- m
for(j in 1:17){
  exposure <- factor_cols[j]
  names_exp <- levels(ukb_analysis[,exposure])[-1]
  m_exp <- matrix(data = NA, ncol = length(names_exp), nrow = 8)
  full_names <- paste(names_exp, exposure, sep="")
  colnames(m_exp) <- full_names
  for(i in 1:8){
    # Fits the model with the categorical variable
    outcome <- ukb_analysis[,i+35]
    model1 <- glm(outcome ~ ukb_analysis[,exposure] + age + sex + ethnic_background, data = ukb_analysis, family = 'binomial')
    #Extracts p-values from the summary output with the exposure name in the title
    rows <- grep("ukb_analysis", rownames(coef(summary(model1))), value = TRUE)
    for(k in 1:length(rows)){
      m_exp[i,k] <- coef(summary(model1))[rows[k],4]
    }
  }
  m2 <- cbind(m2, m_exp)
}
saveRDS(m2, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_pval_only.rds")

#model1 <- glm(cluster_6 ~ able_to_confide + age + sex + ethnic_background, data = ukb_analysis, family = 'binomial')
#summary(model1)

# Assign each variable to a subgroup (for the plot)
Subgroups <- c("Psychosocial", "Behavioural", rep("Environmental", 8), 
               "Sociodemographic", rep("Behavioural", 3), rep("Sociodemographic", 9), 
               rep("Behavioural", 5), rep("Childhood", 2), rep("Psychosocial", 3), 
               rep("Sociodemographic", 5), rep("Behavioural", 2), 
               rep("Sociodemographic", 3), "Psychosocial")

## Dataframe for Manhattan plot
# Take the negative p-value (for the plot)
UNIV_pval <- data.frame(Variable = rep(colnames(m2), 8),
                            Subgroup = rep(Subgroups, 8),
                            Cluster = rep(1:8, each = 44),
                            pval = -as.vector(t(m2)))

saveRDS(UNIV_pval, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_pval.rds")

## Manhattan plot ========
# Just change the cluster number (and cluster number in title)
UNIV_pval %>% filter(Cluster == 2) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0.2, height = 0) +
  geom_abline(intercept = -0.01, slope = 0, colour = 'darkblue') +
  geom_text_repel(aes(label=ifelse(pval > -0.001, as.character(Variable),'')), 
                  hjust=0.6, vjust=-1, size = 2, colour = 'darkblue') +
  labs(title = "Cluster 2") + 
  ylab("P-values") +
  theme(axis.text = element_text(size = 8))

# Stability selection LASSO ===================
ukb_cluster_encoded <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds")
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
ukb_analysis_LASSO <- data.frame(cluster = ukb_analysis$cluster, sex = ukb_analysis$sex,
                                 age = ukb_analysis$age, ethnic_background = ukb_analysis$ethnic_background,
                                 ukb_cluster_encoded)
ukb_analysis_LASSO <- dummy_cols(ukb_analysis_LASSO, select_columns = 'cluster')
ukb_analysis_LASSO$sex <- as.numeric(ukb_analysis_LASSO$sex)
ukb_analysis_LASSO$ethnic_background <- as.numeric(ukb_analysis_LASSO$ethnic_background)
saveRDS(ukb_analysis_LASSO, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis_LASSO.rds")

summary_table <- matrix(data = NA, ncol = 47, nrow = 8)

colnames(summary_table) <- c(colnames())

X <- ukb_analysis_LASSO[,2:48]
Y <- ukb_analysis_LASSO[,56] # 49 to 56 are the cluster columns

## Selected variables ====================
# Prints the variables with p-value < 0.001
set.seed(3142)
#C_selected <- UNIV_pval %>% filter(Cluster == 2) %>% filter(pval > -0.001) %>% reframe(Variable = Variable)
#C1_selected <- c("age", "sex", "ethnicity", C_selected$Variable)

# Stability LASSO
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))
set.seed(3142)
out <- VariableSelection(
  xdata = as.matrix(X)[select,],
  ydata = as.matrix(Y)[select],
  verbose = FALSE,
  penalty.factor = c(0,0,0, rep(1, 44)),
  family = "gaussian",
  n_cat = 3
)

#par(mar = c(10, 5, 1, 1))
png('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/C8_calibration.png', width = 800, height = 600)
CalibrationPlot(out)
dev.off()

selprop <- SelectionProportions(out)
print(selprop)

hat_params <- Argmax(out)
print(hat_params)

png('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/C8_selection.png', width = 800, height = 600)
par(mar = c(10, 5, 1, 1))
plot(selprop, type = "h", lwd = 3, las = 1, xlab = "",
     ylab = "Selection Proportion", xaxt = "n", 
     col = ifelse(selprop >= hat_params[2], yes = "red", no = "grey"), cex.lab = 1.5)
abline(h = hat_params[2], lty = 2, col = "darkred")
for (i in 1:length(selprop)) {
  axis(side = 1, at = i, labels = names(selprop)[i],
       las = 2, col = ifelse(selprop[i] >= hat_params[2],
                             yes = "red", no = "grey"), 
       col.axis = ifelse(selprop[i] >= hat_params[2], yes = "red", no = "grey"))
}
dev.off()


# Logistic regression ===============






