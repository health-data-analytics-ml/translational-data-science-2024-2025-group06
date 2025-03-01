# Penalised regression
# install.packages("fake")
# install.packages("sharp")
library(glmnet)
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(fake))
suppressPackageStartupMessages(library(sharp))
ukb_analysis <- readRDS("/rds/general/project/hda_24-25/live/TDS/Group06/extraction_and_recoding/outputs/Analysis/ukb_analysis.rds")
ukb_analysis$sex <- as.numeric(ukb_analysis$sex)
ukb_analysis$ethnicity <- as.numeric(ukb_analysis$ethnicity)
set.seed(3142)

train <- sample(1:nrow(ukb_analysis), floor(0.8 * nrow(ukb_analysis)))
test <- seq(1, nrow(ukb_analysis))[-train] # gets the 20% test set
select <- sample(train, 0.625*length(train)) # gets the 50% variable selection set
train <- setdiff(train, select) # gets the 30% training set

set.seed(3142)
X <- ukb_analysis[,2:48]
Y <- ukb_analysis[,49]

# Stability selection LASSO
out <- VariableSelection(
  xdata = as.matrix(X)[select, ],
  ydata = as.matrix(Y)[select, ],
  verbose = FALSE,
  penalty.factor = c(0,0,0, rep(1, 44)),
  family = "gaussian"
)

par(mar = c(10, 5, 1, 1))
CalibrationPlot(out)

selprop <- SelectionProportions(out)
print(selprop)
# Basic analysis =============
set.seed(3142)
X <- ukb_analysis[,2:48]
Y <- ukb_analysis[,49]

model_lasso <- cv.glmnet(as.matrix(X)[select, ], as.matrix(Y)[select], alpha = 1, family = "gaussian")
model_lasso_pred = predict(model_lasso, s = model_lasso$lambda.1se, newx = as.matrix(X)[test, ])
mean((model_lasso_pred - as.matrix(Y)[test])^2)

round(coef(model_lasso, s = 'lambda.1se'),digits = 3 )

# Full analysis ================
X <- ukb_analysis[,2:48]
Y <- ukb_analysis[,49]

model_lasso <- cv.glmnet(as.matrix(X)[train, ], as.matrix(Y)[train], alpha = 1, family = "gaussian")

# default k value is nfolds=10
plot(model_lasso)
# It is always 18976 as ridge
length(which(coef(model_lasso)!=0))-1 # 5 are not

model_lasso$lambda.min  # 0.0002277641
model_lasso$lambda.1se  # 0.00490703
min(model_lasso$cvm)  # 0.0188071

id_min <- which(model_lasso$lambda == model_lasso$lambda.min)
model_lasso$cvm[id_min] # 0.0188071 is the same as lambda.min

id_min <- which(model_lasso$lambda == model_lasso$lambda.1se)
model_lasso$cvm[id_min] # 0.0190237

bestlam <- model_lasso$lambda.1se

model_lasso_pred = predict(model_lasso, s = bestlam, newx = as.matrix(X)[test, ])
mean((model_lasso_pred - as.matrix(Y)[test])^2) # 0.01808266

round(cbind(
  coef(model_lasso, s = 'lambda.min'),
  coef(model_lasso, s = 'lambda.1se')),
  digits = 3 )

round(coef(model_lasso, s = 'lambda.1se'),digits = 3 )
