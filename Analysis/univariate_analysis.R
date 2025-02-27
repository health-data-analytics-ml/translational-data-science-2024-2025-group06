# Univariate analysis
# install.packages('qqman')
rm(list = ls())

library(fastDummies)
library(qqman)

gmm_model <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds")
ukb_data <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/ukb_cluster_encoded.rds")
ukb_data <- data.frame(cluster = gmm_model$classification, ukb_data)

# Make toy version
ukb_data <- ukb_data[1:500,]
ukb_data <- dummy_cols(ukb_data, select_columns = 'cluster')

# Fitting a logistic regression to each?
# Add age + sex + ethnicity?

# model1 <- glm(cluster_8 ~ met_score, data = ukb_data, family = 'binomial')
# summary(model1)
# coef(summary(model1))[2,4]

m <- matrix(data = NA, ncol = 44, nrow = 10)
colnames(m) <- colnames(ukb_data[2:45])

for(i in 1:10){
  outcome <- ukb_data[,i+45]
  for(j in 1:44){
    model1 <- glm(outcome ~ ukb_data[,j+1], data = ukb_data, family = 'binomial')
    m[i,j] <- coef(summary(model1))[2,4] # this may change with age etc?
  }
}

# Manhattan plot for each cluster
m_log <- -log(m, base = 10)
Subgroups <- c("Psychosocial", "Behavioural", rep("Environmental", 8), 
               "Sociodemographic", rep("Behavioural", 3), rep("Sociodemographic", 9), 
               rep("Behavioural", 5), rep("Childhood", 2), rep("Psychosocial", 3), 
               rep("Sociodemographic", 5), rep("Behavioural", 2), 
               rep("Sociodemographic", 3), "Psychosocial")

manh_df <- data.frame(Variable = rep(colnames(m_log), 10),
                      Subgroup = rep(Subgroups, 10),
                      Cluster = rep(1:10, each = 44),
                      pval = as.vector(t(m_log)))

manh_df %>% filter(Cluster == 10) %>% ggplot(aes(x = Subgroup, y = pval, label = Variable)) + 
  geom_jitter(width = 0.2, height = 0) +
  geom_abline(intercept = 1.301, slope = 0, colour = 'green') +
  geom_text(aes(label=ifelse(pval > 1.301, as.character(Variable),'')), 
            hjust=1, vjust=-1, size = 3) + 
  labs(title = "Cluster 1") + 
  theme(axis.text = element_text(size = 8))

