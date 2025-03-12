# Runs univariate analysis on proteins and generates tables to use in analysis_plots
rm(list = ls())
library(tidyverse)
library(ggrepel)
library(ggplot2)

# Univariate analysis ==========================================================
# 100% of data used
## Testing out on 10 proteins ==================================================
ukb_proteins_univariate <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_univariate.rds")
ukb_proteins_univariate <- ukb_proteins_univariate[, c(1:13, 1347)]

# Dataframe to store p-values
results <- data.frame(protein = character(),
                      beta = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Logistic regression for each protein
t0 = Sys.time()
for (i in 4:13) { # Columns with proteins
  protein_name <- names(ukb_proteins_univariate)[i]
  model <- glm(cluster_1 ~ ukb_proteins_univariate[[i]] + age + sex + ethnic_background, data = ukb_proteins_univariate, family = 'binomial')
  coef_summary <- summary(model)$coefficients
  beta <- coef_summary[2, 1]
  p_value <- coef_summary[2, 4]
  results = rbind(results, data.frame(protein = protein_name,
                                      beta = beta,
                                      p_value = p_value))
}
t1 = Sys.time()
print(t1 - t0) # Time difference of 18.58812 secs

# Manhattan plot
results$logP <- -log10(results$p_value)
ggplot(results, aes(x = protein, y = logP)) +
  geom_point(size = 3, color = 'blue') +
  geom_hline(yintercept = -log10(0.05), linetype = 'dashed', color = 'red') + # sig threshold, need to account for multiple testing
  theme_minimal() +
  labs(title = 'Manhattan Plot of Univariate Analysis',
       x = 'Protein',
       y = '-log10(p-value)') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Still 10 proteins but vectorise logistic regression ==========================
## Function for logistic regression, grab p-value
protein_indices <- 4:13
logistic <- function(protein_col) {
  model <- glm(cluster_1 ~ ukb_proteins_univariate[[protein_col]] + age + sex + ethnic_background, 
               data = ukb_proteins_univariate, family = 'binomial')
  coef_summary <- summary(model)$coefficients
  data.frame(protein = names(ukb_proteins_univariate)[protein_col],
             estimate = coef_summary[2, 1],
             p_value = coef_summary[2, 4])
}
## Apply function to all protein columns
## Looking at time because curious
t0 = Sys.time()
results_fast <- do.call(rbind, lapply(protein_indices, logistic)) # do.call - list of dfs and combines row wise
t1 = Sys.time()
print(t1 - t0) # Time difference of 15.50716 secs






