rm(list = ls())

library(ggplot2)
library(geomtextpath)
library(viridis)

# Univariate ============
## Manhattan plot (log scale) ========
univariate_log <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/univariate_log.rds")
manhattan_log <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/manhattan_log.rds")
figure_names <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/figure_names.rds")
bonferroni <- -log(0.05/44, base = 10)

manhattan_log <- merge(manhattan_log, figure_names, by = 'Variable', all.x = TRUE)

manhattan_log %>% filter(Cluster == 7) %>% ggplot(aes(x = Subgroup, y = pval, label = figure_variable)) +
  # Show all points
  geom_jitter( aes(color=as.factor(Subgroup)), alpha=1, size=1.7, position = position_jitter(seed = 2)) +
  #geom_abline(intercept = bonferroni, slope = 0, colour = 'red2') +
  geom_hline(yintercept = bonferroni, linetype = "dashed", color = "red")  + 
  geom_text_repel(aes(label=ifelse(pval >= bonferroni, as.character(figure_variable),'')), 
                  size = 2.4, colour = 'black', max.overlaps = Inf, ylim = c(4, 54),
                  position = position_jitter(seed = 2)) +
  scale_color_manual(values = c("#DA589BCC","#E41A1CE6","#33AE5FE6",  
                                "#4D9AD5E6", "#FF7F00E6")) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color='red'),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = 'grey80'),
        panel.grid.major.y = element_line(colour = 'grey80'),
        panel.grid.minor.y = element_line(colour = 'grey80'),
        panel.background = element_rect(fill = 'white')) + 
  labs(title = "Cluster 7: Manhattan Plot of Exposure p-values", caption = "Bonferroni") + 
  ylab(expression(-log[10](italic('p')))) +
  xlab("Exposure Subgroup") +
  ylim(0,54)

ggsave("C7_manhattan.png", path = '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/Manhattan_plots',
       width = 8, height = 6, dpi = 300)

# geom_texthline(yintercept = bonferroni, label = "Bonferroni", hjust = 1.8, linetype = "dashed", vjust = 1.3, color = "red2")

## UpSet plot - univariate ===========
library(UpSetR)
univariate_log <- as.data.frame(t(readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/univariate_log.rds')))
threshold <- -log(0.05/44, base = 10)
for (i in 1:7) {
  colnames(univariate_log)[i] <- paste0('cluster_', i)
  univariate_log[, i] <- ifelse(univariate_log[, i] > threshold, 1, 0)
}

colnames(univariate_log) <- gsub('cluster_', 'Cluster ', colnames(univariate_log))
binary_df <- univariate_log
binary_df[binary_df > 0] <- 1
save_dir <- '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/upset_plot.png'
png(save_dir, width = 1600, height = 1200, res = 300)
upset(binary_df, 
      sets = rev(colnames(binary_df)), # Cluster 1 will print at the top
      keep.order = T,
      order.by = 'freq')
dev.off()

## Barplot - univariate ==========
manhattan_log <- as.data.frame(readRDS('/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/manhattan_log.rds'))
threshold <- -log(0.05/44, base = 10)
barplot <- manhattan_log %>% filter(pval > threshold) %>% group_by(Subgroup, Cluster) %>% summarise(Count = n())

ggplot(barplot, aes(fill=Subgroup, y=Count, x=Cluster)) + 
  geom_bar(position="stack", stat="identity") + 
  labs(title = 'Barchart of Counts of Significant Exposures') +
  scale_fill_manual(values = c("#E41A1CE6", "#DA589BCC", "#33AE5FE6", 
                                "#4D9AD5E6", "#FF7F00E6")) +
  theme_minimal() +
  xlab('Cluster') +
  scale_x_continuous(breaks=(seq(1,7,1))) +
  theme(axis.text.x = element_text(angle = 0, size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"))

ggsave("Uni_exposure_barplot.png", path = '/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals',
       width = 8, height = 5, dpi = 300)

# Table
manhattan_log %>% filter(pval > threshold) %>% group_by(Cluster) %>% summarise(Count = n())


# Stability LASSO =============
## Tile map =========
# Just the selected variables
figure_names <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/figure_names.rds")
cluster_names <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/cluster_names.rds")
selected_vars <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/selected_vars.rds")

# Get data into right format
selected_longer <- ifelse(selected_vars >= 0.97, 1, 0)
selected_longer <- as.data.frame(selected_longer) %>% 
  mutate(row_sum = rowSums(selected_longer)) %>% 
  filter(row_sum > 0)

selected_longer <- selected_longer %>% mutate(Variable = rownames(selected_longer)) %>% 
  pivot_longer(colnames(selected_vars), names_to = "Cluster", values_to = "Selection Proportion")

selected_longer <- merge(selected_longer, figure_names, by = 'Variable', 
                         all.x = TRUE)
selected_longer <- merge(selected_longer, cluster_names, by = 'Cluster', 
                         all.x = TRUE)

selected_longer <- selected_longer %>% arrange(desc(row_sum))


# selected_longer <- selected_longer[,-8] %>% 
#   mutate(Variable = rownames(selected_longer)) %>% 
#   pivot_longer(colnames(selected_vars), names_to = "Cluster", values_to = "Selection Proportion")
# 
# selected_longer <- merge(selected_longer, figure_names, by = 'Variable', 
#                          all.x = TRUE, sort = FALSE)
# selected_longer <- merge(selected_longer, cluster_names, by = 'Cluster', 
#                          all.x = TRUE)


p <- ggplot(selected_longer, aes(x = fct_inorder(figure_variable), y = cluster_name, fill = as.factor(`Selection Proportion`))) +
  theme_minimal() +
  geom_tile(color = "white", lwd = 0.5, linetype = 1) + 
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black", face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  scale_fill_manual(values = c("grey89", "dodgerblue"), 
                    name = '', 
                    labels = c('Not selected', 'Selected')) + 
  scale_y_discrete(limits=rev) +
  labs(title = 'Tilemap of Variable Selection Proportions') +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = 'right') +
  ylab('')
  #guides(fill = guide_legend(nrow = 2))
p
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/sel_prop_tilemap.png', plot = p, width = 10, height = 5, dpi = 300)

# name = expression(paste("Selection Proportion", (pi))), 
# labels = c(expression(paste(pi," < 0.97")), expression(paste(pi," > 0.97"))))

# Logistic - tile map ================
log_output <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/logistic_output.rds")
figure_names <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/figure_names.rds")
cluster_names <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/cluster_names.rds")

colnames(log_output)[3] <- 'Cluster'
log_output <- merge(log_output, cluster_names, by = 'Cluster', all.x = TRUE)
log_output <- merge(log_output, figure_names, by = 'Variable', 
                         all.x = TRUE)

log_output <- log_output %>% group_by(Variable) %>% mutate('Popularity' = n()) %>% arrange(desc(Popularity))
log_output <- cbind(log_output, "OR_colour" = rep(1, nrow(log_output)))
for(i in 1:nrow(log_output)){
  if (log_output$Odds_ratio[i] >= 1) {
    log_output$OR_colour[i] = "OR \u2265 1"
  } else {
    log_output$OR_colour[i] = "OR < 1"
  }
}
log_output <- log_output %>% mutate(significance = ifelse(p_value < 0.05, TRUE, FALSE))

log_output$Odds_ratio_capped <- ifelse(log_output$Odds_ratio>5, 5, log_output$Odds_ratio)


p <- ggplot(log_output, aes(x = fct_inorder(figure_variable), y = cluster_name, fill = Odds_ratio_capped)) +
  geom_tile(color = "black", lwd = 0.1, linetype = 1) + 
  geom_text(aes(label = Odds_ratio), color = "black", size = 3, fontface = "bold") +  
  scale_fill_gradientn(colours = c("dodgerblue", "white", "brown2"), 
                       limits = c(0, ifelse(max(log_output$Odds_ratio_capped) > 1, max(log_output$Odds_ratio_capped), 1)),
                       values = c(0, ifelse(max(log_output$Odds_ratio_capped) > 1, 1/max(log_output$Odds_ratio_capped), 1), 1),
                       name = "Odds Ratio",
                       breaks = c(0, 1, 2, 3, 4, 5),  # Show full range in legend
                       labels = c("0", "1", "2", "3", "4", "\u2265 5")) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black", face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 13, face = "bold")) +
  xlab('') +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_y_discrete(limits=rev) +
  #geom_tile(aes(colour = significance), fill = '#00000000', linewidth = 1) +
  #scale_color_manual(values = c('#00000000', 'black')) +
  #theme(legend.position="none") +
  labs(title = 'Tilemap of Logistic Regression Odds Ratios',
       legend = "Odds Ratio") +
  ylab('')
p
ggsave('/rds/general/project/hda_24-25/live/TDS/Group06/Scripts/Visualisations/Analysis_visuals/log_regression_tilemap.png', plot = p, width = 15, height = 6, dpi = 300)


# log_output2$p_value <- ifelse((log_output$Odds_ratio >= 1), log_output2$p_value, -log_output2$p_value)

# Summary tilemap ===============
summary <- merge(manhattan_log, log_output)
