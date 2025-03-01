# Univariate analysis
# Load data for plotting p-values (original scale)
manh_original <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_pval.rds")

# 'Manhattan plot' just change the Cluster number
manh_original %>% filter(Cluster == 1) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0.2, height = 0) +
  geom_abline(intercept = -0.01, slope = 0, colour = 'darkblue') +
  geom_text_repel(aes(label=ifelse(pval > -0.01, as.character(Variable),'')), 
                  hjust=0.6, vjust=-1, size = 2, colour = 'darkblue') +
  labs(title = "Cluster 1") + 
  ylab("P-values") +
  theme(axis.text = element_text(size = 8))


# Full logistic regression?
manh_original %>% filter(Cluster == 2) %>% filter(pval > -0.001) %>% reframe(Variable = Variable)

manh_df <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_logistic_pval.rds")

# p-value for cut off
-log(0.001, base = 10) # 1.30103
-log((0.05/44), base = 10) # bonferroni  2.944483

manh_df %>% filter(Cluster == 1) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0, height = 0) +
  geom_abline(intercept = 1.301, slope = 0, colour = 'darkblue') +
  geom_abline(intercept = 2.944, slope = 0, colour = 'orange') +
  geom_text_repel(aes(label=ifelse((2.944 > pval & pval >= 1.301), as.character(Variable),'')), 
                  hjust=1, vjust=-1, size = 3, colour = 'darkblue') +
  geom_text_repel(aes(label=ifelse(pval > 2.944, as.character(Variable),'')), 
                  hjust=1, vjust=-1, size = 3, colour = 'orange') +
  labs(title = "Cluster 1") + 
  ylab("-log(p-values)") +
  theme(axis.text = element_text(size = 8))

manh_df %>% filter

# Without base 10
manh_original <- data.frame(Variable = rep(colnames(m), 10),
                            Subgroup = rep(Subgroups, 10),
                            Cluster = rep(1:10, each = 44),
                            pval = -as.vector(t(m)))
saveRDS(manh_original, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_pval.rds")










# install.packages('qqman')
rm(list = ls())

library(fastDummies)
library(qqman)
library(tidyverse)
library(ggrepel)

gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_cluster_encoded <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds")
ukb_final_reduced <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_final_reduced.rds")
ukb_analysis <- data.frame(cluster = gmm_model$classification, age = ukb_final_reduced$age,
                       sex = ukb_final_reduced$sex,
                       ethnicity = ukb_final_reduced$ethnic_background, ukb_cluster_encoded)
ukb_analysis <- ukb_analysis %>% mutate(ethnicity = ifelse((ethnicity == "British" | 
                                                      ethnicity == "Black or Black British" |
                                                      ethnicity == "Asian or Asian British"), "British", "Other_ethnicity"))

ukb_analysis$ethnicity <- as.factor(ukb_analysis$ethnicity)
saveRDS(ukb_analysis, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")

# Make toy version
# ukb_data <- ukb_data[1:500,]
ukb_data <- dummy_cols(ukb_data, select_columns = 'cluster')

# Fitting a logistic regression to each?
# Add age + sex + ethnicity?
ukb_data <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")

 model1 <- glm(cluster_8 ~ met_score + age + sex + ethnicity, data = ukb_data, family = 'binomial')
summary(model1)
 coef(summary(model1))[2,4]
rm(model1)


m <- matrix(data = NA, ncol = 44, nrow = 10)
colnames(m) <- colnames(ukb_data[5:48])

for(i in 1:10){
  outcome <- ukb_data[,i+48]
  for(j in 1:44){
    model1 <- glm(outcome ~ ukb_data[,j+1] + age + sex + ethnicity, data = ukb_data, family = 'binomial')
    m[i,j] <- coef(summary(model1))[2,4]
  }
}

# Manhattan plot for each cluster
m_log <- -log(m, base = 10)
m_log[m_log == Inf] <- NA
Subgroups <- c("Psychosocial", "Behavioural", rep("Environmental", 8), 
               "Sociodemographic", rep("Behavioural", 3), rep("Sociodemographic", 9), 
               rep("Behavioural", 5), rep("Childhood", 2), rep("Psychosocial", 3), 
               rep("Sociodemographic", 5), rep("Behavioural", 2), 
               rep("Sociodemographic", 3), "Psychosocial")

manh_df <- data.frame(Variable = rep(colnames(m_log), 10),
                      Subgroup = rep(Subgroups, 10),
                      Cluster = rep(1:10, each = 44),
                      pval = as.vector(t(m_log)))

saveRDS(manh_df, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_logistic_pval.rds")

manh_df <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_logistic_pval.rds")

# p-value for cut off
-log(0.001, base = 10) # 1.30103
-log((0.05/44), base = 10) # bonferroni  2.944483

manh_df %>% filter(Cluster == 1) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0, height = 0) +
  geom_abline(intercept = 1.301, slope = 0, colour = 'darkblue') +
  geom_abline(intercept = 2.944, slope = 0, colour = 'orange') +
  geom_text_repel(aes(label=ifelse((2.944 > pval & pval >= 1.301), as.character(Variable),'')), 
            hjust=1, vjust=-1, size = 3, colour = 'darkblue') +
  geom_text_repel(aes(label=ifelse(pval > 2.944, as.character(Variable),'')), 
            hjust=1, vjust=-1, size = 3, colour = 'orange') +
  labs(title = "Cluster 1") + 
  ylab("-log(p-values)") +
  theme(axis.text = element_text(size = 8))

manh_df %>% filter

# Without base 10
manh_original <- data.frame(Variable = rep(colnames(m), 10),
                      Subgroup = rep(Subgroups, 10),
                      Cluster = rep(1:10, each = 44),
                      pval = -as.vector(t(m)))
saveRDS(manh_original, "/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/UNIV_pval.rds")


manh_original %>% filter(Cluster == 1) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0.2, height = 0) +
  geom_abline(intercept = -0.01, slope = 0, colour = 'darkblue') +
  geom_text_repel(aes(label=ifelse(pval > -0.01, as.character(Variable),'')), 
            hjust=0.6, vjust=-1, size = 2, colour = 'darkblue') +
  labs(title = "Cluster 1") + 
  ylab("P-values") +
  theme(axis.text = element_text(size = 8))


# Full logistic regression?
manh_original %>% filter(Cluster == 2) %>% filter(pval > -0.001) %>% reframe(Variable = Variable)









