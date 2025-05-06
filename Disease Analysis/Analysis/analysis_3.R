library (tidyverse)

# Reading in data ---------------------------------------------------------

# Disease data
ad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Alzheimer/Outputs/output_final.rds')
ckd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Chronic_Kidney_Disease/Outputs/output_final.rds')
cad <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Coronary_Artery_Disease/Outputs/output_final.rds')
diabetes <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Diabetes/Outputs/output_final.rds')
pd <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Definitions/Parkinson/Outputs/output_final.rds')

# Exposure and cluster data 
data_read <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Clustering/gmm_model.rds') 
data <- data_read$data
classification <- data_read$classification

# Joining classification and exposure data
cluster_membership <- cbind(data, cluster = classification)
cluster_membership <- as.data.frame(cluster_membership)

# Adding ID, Age, Ethnic Background, and Sex
ukb_analysis <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds')
ukb_analysis$id <- rownames(ukb_analysis)

cluster_membership$id <- rownames(ukb_analysis)

exposures_merged <- cluster_membership %>%
  left_join(ukb_analysis %>% dplyr::select(id,sex, ethnic_background, age), 
            by = 'id')
  
# Reading in proteins
proteins <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_proteins_lasso.rds")
selected_proteins <- readRDS('/rds/general/user/cat24/projects/hda_24-25/live/TDS/Group06/Scripts/Disease Analysis/Analysis/selected_proteins.rds')



# Dropping exposures ------------------------------------------------------
selected_exposures <- readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_variables.rds')
selected_exposures <- selected_exposures %>% dplyr::select(-Variable)
selected_exposures <- as.data.frame(t(selected_exposures))

cols_all_na <- names(selected_exposures)[sapply(selected_exposures, function(x) all(is.na(x)))]

exposures_dropped <- exposures_merged %>% 
  dplyr::select(-any_of(cols_all_na))


# Cleaning proteins ------------------------------------------------------
select_proteins_list <- selected_proteins %>%
  mutate(protein_list = str_split(Variables, " \\+ ")) %>%
  unnest(protein_list) %>%
  distinct(protein_list) %>%
  pull(protein_list)

proteins_dropped <- proteins %>%
  dplyr::select(any_of(select_proteins_list))

proteins_dropped <- proteins_dropped %>% dplyr::select(-c(age, sex, ethnic_background))



# Merging exposure and proteins -------------------------------------------

exposures_proteins_merged <-  bind_cols(exposures_dropped, proteins_dropped)



# Making disease datasets -------------------------------------------------

# Joining proteins and exposures to incidence

ad_df <- exposures_proteins_merged %>% left_join(ad %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

ckd_df <- exposures_proteins_merged %>% left_join(ckd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

cad_df <- exposures_proteins_merged %>% left_join(cad %>% dplyr::select(eid, incident_case), by = c("id" = "eid"))

pd_df <- exposures_proteins_merged %>% left_join(pd %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

diabetes_df <- exposures_proteins_merged %>% left_join(diabetes %>% dplyr::select(eid, incident_case), by = c("id" = "eid")) 

disease_df_list <- list(
  ad_df = ad_df,
  pd_df = pd_df,
  ckd_df = ckd_df,
  cad_df = cad_df,
  diabetes_df = diabetes_df
)


# Removing NAs
new_list <- list()
for (i in seq_along(disease_df_list)) {
  new_list[[i]] <- na.omit(disease_df_list[[i]])
}

ad_df <- new_list[[1]]
pd_df <- new_list[[2]]
ckd_df <- new_list[[3]]
cad_df <- new_list[[4]]
diabetes_df <- new_list[[5]]



disease_df_list <- list(
  AD = ad_df,
  PD = pd_df,
  CKD = ckd_df,
  CAD = cad_df,
  Diabetes = diabetes_df
)



# Logistic Regression with Clusters ---------------------------------------

# Prepare
diseases <- names(disease_df_list)
n_clust  <- 7

# Allocate OR matrix (from before) and the new significance matrix
OR_mat  <- matrix(NA, nrow = length(diseases), ncol = n_clust,
                  dimnames = list(diseases, paste0("Cluster", 1:n_clust)))
sig_mat <- matrix(0,   nrow = length(diseases), ncol = n_clust,
                  dimnames = list(diseases, paste0("Cluster", 1:n_clust)))

# Loop over diseases
for(i in seq_along(diseases)) {
  df <- disease_df_list[[diseases[i]]]
  df$cluster <- factor(df$cluster)
  contrasts(df$cluster) <- contr.sum(n_clust)
  
  # Fit the model
  fit <- glm(incident_case ~ cluster + age + sex + ethnic_background,
             family = binomial(link = "logit"), data = df)
  
  # Extract the six cluster betas and their p‑values
  smry  <- summary(fit)$coefficients
  beta_names <- paste0("cluster", 1:(n_clust-1))
  betas  <- smry[beta_names, "Estimate"]
  pvals  <- smry[beta_names, "Pr(>|z|)"]
  
  # Compute cluster 7’s beta (not directly tested) and its OR
  beta7 <- -sum(betas)
  
  # Fill OR_mat
  ORs     <- exp(c(betas, beta7))
  OR_mat[i, ] <- ORs
  
  # Fill significance matrix: 1 if p<0.05, else 0
  sig_mat[i, 1:(n_clust-1)] <- as.integer(pvals < 0.05)
  sig_mat[i, n_clust]      <- 0   # cluster7 is implicit; set to 0
}

# Inspect
OR_mat
sig_mat



# Plotting the Odds Ratios ------------------------------------------------

library(gplots)

# Define diverging palette
heat_colors <- colorRampPalette(c("dodgerblue", "white", "brown2"))(100)

# Compute breaks ensuring 1 is at the midpoint (with 101 breakpoints for 100 colors)
min_val <- min(OR_mat, na.rm = TRUE)
max_val <- max(OR_mat, na.rm = TRUE)
breaks <- c(seq(min_val, 1, length.out = 51), seq(1, max_val, length.out = 51)[-1])

heatmap.2(
  OR_mat,
  col         = heat_colors,
  breaks      = breaks,
  trace       = "none",
  density.info= "none",
  margins     = c(5,15),
  key.title = '',
  key.xlab = "Odds Ratio",
  key         = TRUE,
  dendrogram  = "none",
  Rowv        = FALSE,
  Colv        = FALSE,
  labRow = c(expression(bold(AD) * " (450; 41,698)"),
             expression(bold(PD) * " (1953; 39,518)"),
             expression(bold(CKD) * " (1659; 40,429)"),
             expression(bold(CAD) * " (11,129; 15,380)"),
             expression(bold(Diabetes) * " (590; 41,646)")),      # Disease names as row labels
  labCol = c(expression(bold('1')),
             expression(bold('2')),
             expression(bold('3')),
             expression(bold('4')),
             expression(bold('5')),
             expression(bold('6')),
             expression(bold('7'))),
  cellnote    = round(OR_mat, 2),
  notecol     = "black",
  notecex     = 0.8,
  cexRow = 1.3,
  
  # THIS IS THE MAGIC PART:
  add.expr = {
    nr <- nrow(OR_mat)
    nc <- ncol(OR_mat)
    for(i in 1:nr) {
      for(j in 1:nc) {
        if(sig_mat[i, j] == 1) {
          # draw a box around cell [i,j]
          # x runs 1…nc left→right, y runs 1…nr bottom→top, 
          # but our row1 is at the *top*, so we flip: y = nr - i + 1
          rect(
            xleft   = j - 0.5,
            ybottom = nr - i + 0.5,
            xright  = j + 0.5,
            ytop    = nr - i + 1.5,
            border  = "black",
            lwd     = 2
          )
        }
      }
    }
  }
)





# AUC 1 -------------------------------------------------------------------

library(pROC)
disease <- c("AD", "PD", "CKD", "CAD", "Diabetes")

AUC_results_1 <- matrix(NA, nrow = 5, ncol = 1, dimnames = list(disease, c('AUC')))
                        
                        

# Plotting AUC
roc_list <- vector("list", length(disease_df_list))
names(roc_list) <- disease  # assume `disease` is your vector of names

for(i in seq_along(disease_df_list)) {
  df <- disease_df_list[[i]] %>%
    mutate(cluster = factor(cluster))
  contrasts(df$cluster) <- contr.sum(7)
  
  set.seed(1342)
  train_idx <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))
  train_df <- df[train_idx, ]
  test_df  <- df[-train_idx, ]
  
  fit <- glm(incident_case ~ cluster + age + sex + ethnic_background,
             family = binomial, data = train_df)
  preds <- predict(fit, newdata = test_df, type = "response")
  
  roc_obj <- roc(test_df$incident_case, preds)
  roc_list[[i]] <- roc_obj
  AUC_results_1[disease[i], "AUC"] <- auc(roc_obj)
}

# 1) Build a combined data.frame of all ROC curves
roc_df <- do.call(rbind, lapply(names(roc_list), function(dz) {
  roc_obj <- roc_list[[dz]]
  data.frame(
    disease    = dz,
    fpr        = 1 - roc_obj$specificities,
    tpr        =    roc_obj$sensitivities,
    stringsAsFactors = FALSE
  )
}))

# 2) Compute AUC labels for the legend
auc_vals   <- sapply(roc_list, auc)
auc_labels <- paste0(names(auc_vals), " (AUC=", sprintf("%.2f", auc_vals), ")")

# 3) Plot with ggplot2
plot1 <- ggplot(roc_df, aes(x = fpr, y = tpr, color = disease)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60") +
  scale_color_discrete(labels = auc_labels) +
  labs(
    title = "ROC Curves by Disease (Cluster)",
    x     = "False Positive Rate",
    y     = "True Positive Rate",
    color = "Disease (AUC)"
  ) +
  theme_minimal()



# ggsave(filename = "/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Analysis/Plots/roc_curves1.pdf",  # path + name
#        plot     = plot1,                # which ggplot object
#        width    = 8,                # in inches
#        height   = 6,
#        dpi      = 300)              # resolution

# Making new disease df for just exposures --------------------------------
ad_df_exp       <- ad_df       %>% dplyr::select(1:22, last_col()) %>% dplyr::select(-id)
cad_df_exp      <- cad_df      %>% dplyr::select(1:22, last_col()) %>% dplyr::select(-id)
ckd_df_exp      <- ckd_df      %>% dplyr::select(1:22, last_col()) %>% dplyr::select(-id)
diabetes_df_exp <- diabetes_df %>% dplyr::select(1:22, last_col()) %>% dplyr::select(-id)
pd_df_exp       <- pd_df       %>% dplyr::select(1:22, last_col()) %>% dplyr::select(-id)


# Logistic with exposures and cluster -------------------------------------

disease <- c("AD", "PD", "CKD", "CAD", "Diabetes")
AUC_results_2 <- matrix(NA, nrow = 5, ncol = 1, 
                        dimnames = list(disease,
                                        c('AUC')))
                        
                        

# Plotting AUC
disease_df_list2 <-  list(
  AD = ad_df_exp,
  PD = pd_df_exp,
  CKD = ckd_df_exp,
  CAD = cad_df_exp,
  Diabetes = diabetes_df_exp
)

# make sure contrasts are applied
for(i in seq_along(disease_df_list2)){
  disease_df_list2[[i]]$cluster <- factor(disease_df_list2[[i]]$cluster)
  contrasts(disease_df_list2[[i]]$cluster) <- contr.sum(7)
}

# init
set.seed(1342)
roc_list2    <- vector("list", length(disease_df_list2))
names(roc_list2) <- disease
AUC_results_2 <- matrix(NA, nrow=5, ncol=1, dimnames=list(disease,"AUC"))

# loop
for(i in seq_along(disease_df_list2)) {
  df    <- disease_df_list2[[i]]
  train <- sample(seq_len(nrow(df)), floor(0.8*nrow(df)))
  fit   <- glm(incident_case ~ ., family=binomial, data=df[train, ])
  preds <- predict(fit, df[-train, ], type="response")
  roc_o <- roc(df$incident_case[-train], preds)
  
  roc_list2[[i]]      <- roc_o
  AUC_results_2[i, 1] <- auc(roc_o)
}

# build plot data from roc_list2
roc_df2 <- do.call(rbind, lapply(names(roc_list2), function(dz) {
  r <- roc_list2[[dz]]
  data.frame(
    disease=dz,
    fpr=1-r$specificities,
    tpr=r$sensitivities
  )
}))

# Get the numeric AUCs straight from roc_list3
auc_vals2 <- sapply(roc_list2, auc)

# Build the human‑readable labels
auc_labels2 <- sprintf("%s (AUC = %.2f)",
                       names(auc_vals2),
                       auc_vals2)

plot2 <- ggplot(roc_df2, aes(fpr, tpr, color=disease)) +
  geom_line(size = 1) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  scale_color_discrete(labels=auc_labels2) +
  labs(
    title = "ROC Curves by Disease (Cluster & Exposure)",
    x     = "False Positive Rate",
    y     = "True Positive Rate",
    color = "Disease (AUC)"
  ) + theme_minimal()
  


ggsave(filename = "/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Analysis/Plots/roc_curves2.pdf",  # path + name
       plot     = plot2,                # which ggplot object
       width    = 8,                # in inches
       height   = 6,
       dpi      = 300)              # resolution

# Logistic with exposures and proteins ------------------------------------
ad_df <- ad_df %>% dplyr::select(-id)
cad_df <- cad_df %>% dplyr::select(-id)
ckd_df <- ckd_df %>% dplyr::select(-id)
diabetes_df <- diabetes_df %>% dplyr::select(-id)
pd_df <- pd_df %>% dplyr::select(-id)

AUC_results_3 <- matrix(NA, nrow = 5, ncol = 1, 
                        dimnames = list(disease,
                                        c('AUC')))
                        
                        
# Plotting AUC
disease_df_list3 <- list(
  AD = ad_df,
  PD = pd_df,
  CKD = ckd_df,
  CAD = cad_df,
  Diabetes = diabetes_df
)
# 1) Prepare the data list & apply zero‐sum contrasts
disease_df_list3 <- list(
  AD       = ad_df,
  PD       = pd_df,
  CKD      = ckd_df,
  CAD      = cad_df,
  Diabetes = diabetes_df
)
for(i in seq_along(disease_df_list3)) {
  disease_df_list3[[i]]$cluster <- factor(disease_df_list3[[i]]$cluster)
  contrasts(disease_df_list3[[i]]$cluster) <- contr.sum(7)
}

# 2) Initialize storage
disease       <- names(disease_df_list3)      # c("AD","PD","CKD","CAD","Diabetes")
set.seed(1342)                                # once, before all sampling
roc_list3     <- vector("list", length(disease_df_list3))
names(roc_list3) <- disease
AUC_results_3 <- matrix(NA, nrow = length(disease), ncol = 1,
                        dimnames = list(disease, "AUC"))

# 3) Loop: fit, predict, store ROC/AUC
for(i in seq_along(disease_df_list3)) {
  df        <- disease_df_list3[[i]]
  train_idx <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))
  train_df  <- df[train_idx, ]
  test_df   <- df[-train_idx, ]
  
  fit   <- glm(incident_case ~ ., family = binomial, data = train_df)
  preds <- predict(fit, newdata = test_df, type = "response")
  
  roc_obj <- roc(test_df$incident_case, preds)
  roc_list3[[i]]      <- roc_obj
  AUC_results_3[i, 1] <- auc(roc_obj)
}

# 4) Build a combined ROC‐curve data.frame
roc_df3 <- do.call(rbind, lapply(names(roc_list3), function(dz) {
  r <- roc_list3[[dz]]
  data.frame(
    disease = dz,
    fpr     = 1 - r$specificities,
    tpr     =     r$sensitivities
  )
}))

# Get the numeric AUCs straight from roc_list3
auc_vals3 <- sapply(roc_list3, auc)

# Build the human‑readable labels
auc_labels3 <- sprintf("%s (AUC = %.2f)",
                       names(auc_vals3),
                       auc_vals3)

# 6) Plot with ggplot2
plot3 <- ggplot(roc_df3, aes(x = fpr, y = tpr, color = disease)) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_discrete(labels = auc_labels3) +
  labs(
    title = "ROC Curves by Disease (Cluster, Exposure & Protein)",
    x     = "False Positive Rate",
    y     = "True Positive Rate",
    color = "Disease (AUC)"
  ) + theme_minimal()


# ggsave(filename = "/rds/general/project/hda_24-25/live/TDS/Group06/outcome_definition/Analysis/Plots/roc_curves3.pdf",  # path + name
#        plot     = plot3,                # which ggplot object
#        width    = 8,                # in inches
#        height   = 6,
#        dpi      = 300)              # resolution






