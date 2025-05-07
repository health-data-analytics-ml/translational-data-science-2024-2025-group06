# ------------------------------------------------------------------------------
# Purpose: This script plots participant counts per cluster and creates table 1s
# ------------------------------------------------------------------------------

# Libraries ====================================================================
rm(list = ls())
library(dplyr)
library(ggplot2)
library(tableone)
library(table1)

# Create dataframe with non-scaled, non-one hot encoded data ===================
gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
ukb_clustered <- data.frame(cluster = gmm_model$classification, ukb_final_reduced)
ukb_clustered <- rename(ukb_clustered, 'ethnicity' = 'ethnic_background')
ukb_clustered <- ukb_clustered %>% mutate(ethnicity = ifelse((ethnicity == "British" | 
                                                      ethnicity == "Black or Black British" |
                                                      ethnicity == "Asian or Asian British"), "British", "Other_ethnicity"))
ukb_clustered$ethnicity <- as.factor(ukb_clustered$ethnicity)
saveRDS(ukb_clustered, '/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_clustered.rds')

# Counts and percentages for each cluster ======================================
selected_colors <- c("#E41A1C", "#FFFF33", "#4DAF4A", "#999999", "#984EA3", "#377EB8", "#A65628", "#F781BF", "#FF7F00", "#66C2A5")
cluster_counts <- table(ukb_clustered$cluster)
cluster_percentages <- round(prop.table(cluster_counts) * 100, 2)
cluster_summary <- data.frame(cluster = as.factor(names(cluster_counts)),
                              count = as.numeric(cluster_counts),
                              percentage = as.numeric(cluster_percentages))
print(cluster_summary, row.names = FALSE)

ggplot(ukb_clustered, aes(x = factor(cluster), fill = factor(cluster))) +
  geom_bar(color = "black", width = 1, position = "stack") +
  geom_text(data = cluster_summary,
            aes(x = cluster, y = count, label = paste0(percentage, '%')),
            vjust = -0.5, size = 2, fontface = 'bold') +
  labs(title = "Distribution of Clusters",
       x = "Cluster",
       y = "Count") +
  scale_x_discrete(limits = as.character(1:7)) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Giving prettier names for columns ============================================
colnames(ukb_clustered) <- gsub('_', ' ', colnames(ukb_clustered))
colnames(ukb_clustered) <- tools::toTitleCase(colnames(ukb_clustered))
ukb_clustered <- rename(ukb_clustered, 'Neuroticism Score' = 'Neuro Score')

# Table 1 for each cluster =====================================================
sociodemographic <- c('Cluster',
                      'Sex',
                      'Age',
                      'Ethnicity',
                      'Multiple Deprivation Index',
                      'Household Income',
                      'Qualifications',
                      'Employment Status',
                      'Own Rent',
                      'Num Household',
                      'Vehicles Household')
length(sociodemographic) # 10 (plus cluster)
environmental <- c('Cluster',
                   'No2 2010',
                   'Pm10',
                   'Pm2.5',
                   'Traffic Intensity',
                   'Inv Dis Maj Road',
                   'Greenspace 1000m',
                   'Water 1000m',
                   'Coast Distance',
                   'Gas Cooker',
                   'Gas Fire',
                   'Solid Fire')
length(environmental) # 12 (plus cluster)
other <- c('Cluster',
           'Alcohol Intake',
           'Alcohol 10 Years',
           'Smoking Status',
           'Pack Years',
           'Breastfed',
           'Maternal Smoking',
           'Anxiety',
           'Able To Confide',
           'Neuro Score',
           'Distress Score',
           'Sleep Score',
           'Diet Score',
           'Met Score')
length(other) # 13 (plus cluster)

# Double check I used all variables
38 == length(sociodemographic) + length(environmental) + length(other)

# Table 1s (all variables) =====================================================
## Defining function to add p-values
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform anova
    p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

## Defining function for continuous variables
my.render.cont <- function(x) {
  if (!is.numeric(x)) return("")
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  median_val <- median(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  c("",
    "Mean (SD)" = sprintf("%s (%s)", round(mean_val, 2), round(sd_val, 2)),
    "Median (Min, Max)" = sprintf("%s (%s, %s)", round(median_val, 1), round(min_val, 1), round(max_val, 1))
  )
}

## Environment
table1(~ `No2 2010` + Pm10 + Pm2.5 + `Traffic Intensity` + `Inv Dis Maj Road` + `Greenspace 1000m` +
         `Water 1000m` + `Coast Distance` + `Gas Cooker` + `Gas Fire` + `Solid Fire`
       | as.factor(Cluster), data = ukb_clustered, render.continuous = my.render.cont,
       extra.col=list(`P-value`=pvalue))

## Sociodemographic
table1(~ Sex + Age + Ethnicity + `Multiple Deprivation Index` + `Household Income` +
         Qualifications + `Employment Status` + `Own Rent` + `Num Household` + `Vehicles Household`
       | as.factor(Cluster), data = ukb_clustered, extra.col=list(`P-value`=pvalue))

## Other
table1(~ `Alcohol Intake` + `Alcohol 10 Yrs` + `Smoking Status` + `Pack Years` + `Breastfed` + 
         `Maternal Smoking` + Anxiety + `Distress Score` + `Sleep Data` + `Diet Score` + `Met Score`
       | Cluster, data = ukb_clustered, extra.col=list(`P-value`=pvalue))

# Selecting only interesting variables for final presentation ==================
## Picked which ones to keep based on unbalanced classes
table1(~ `Sex` + `Age` + `Ethnicity` + `Multiple Deprivation Index` + `Household Income` + `Employment Status` +
       `Own Rent` + `Vehicles Household` + `Gas Cooker` + `Solid Fire`
       | as.factor(Cluster), data = ukb_clustered, extra.col=list(`P-value`=pvalue))

table1(~ `No2 2010` + `Pm10` + `Traffic Intensity` + `Distance to Major Road` + `Greenspace 1000m` + `Water 1000m` + 
         `Smoking Status` + `Pack Years` + `Anxiety` + `Neuroticism Score` + `Met Score`
       | as.factor(Cluster), data = ukb_clustered, render.continuous = my.render.cont, extra.col=list(`P-value`=pvalue))
