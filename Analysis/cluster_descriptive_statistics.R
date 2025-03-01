rm(list = ls())
library(dplyr)
library(ggplot2)
library(tableone)

# Create dataframe with non-scaled, non-one hot encoded data ===================
gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
ukb_clustered <- data.frame(cluster = gmm_model$classification,
                            ukb_final_reduced)
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
cluster_summary <- data.frame(Cluster = as.integer(names(cluster_counts)),
                              Count = as.numeric(cluster_counts),
                              Percentage = as.numeric(cluster_percentages))
print(cluster_summary, row.names = FALSE)

ggplot(ukb_clustered, aes(x = factor(cluster), fill = factor(cluster))) +
  geom_bar(color = "black", width = 1, position = "stack") +
  labs(title = "Distribution of Clusters",
       x = "Cluster",
       y = "Count") +
  scale_x_discrete(limits = as.character(1:10)) +
  scale_fill_manual(values = selected_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Table 1 for each cluster =====================================================
sociodemographic <- c('sex',
                      'age',
                      'ethnicity',
                      'multiple_deprivation_index',
                      'household_income',
                      'qualifications',
                      'employment_status',
                      'own_rent',
                      'num_household',
                      'vehicles_household')
length(sociodemographic) # 10
environmental <- c('no2_2010',
                   'pm10',
                   'pm2.5',
                   'traffic_intensity',
                   'inv_dis_maj_road',
                   'greenspace_1000m',
                   'water_1000m',
                   'coast_distance',
                   'gas_cooker',
                   'gas_fire',
                   'gas_solid',
                   'solid_fire')
length(environmental) # 12
other <- c('alcohol_intake',
           'alcohol_10_years',
           'smoking_status',
           'pack_years',
           'breastfed',
           'maternal_smoking',
           'anxiety',
           'able_to_confide',
           'neuro_score',
           'distress_score',
           'sleep_score',
           'diet_score',
           'met_score')
length(other) # 13
# Double check I used all variables
35 == length(sociodemographic) + length(environmental) + length(other)

## Table 1
numeric_cols <- names(ukb_clustered)[sapply(ukb_clustered, function(x) is.numeric(x) | is.integer(x))]
table1 <- CreateTableOne(numeric_cols,
                         strata = 'cluster',
                         data = ukb_clustered, 
                         factorVars = cat_columns)
table1
## Put into overleaf to display

## Table 1 for categorical variables
cat_columns <- c("household_income", "alcohol_intake", "alcohol_10_yrs")
cat_columns <- names(ukb_clustered)[sapply(ukb_clustered, is.factor)]
table1 <- CreateTableOne(cat_columns,
                         strata = 'cluster',
                         data = ukb_clustered, 
                         factorVars = cat_columns)
table1





