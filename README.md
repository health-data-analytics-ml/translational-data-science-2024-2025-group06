# Expotype Clustering
## Overview
This project identifies exposure-driven clusters (expotypes) in UK Biobank participants, characterises their proteomic signatures, and examines associations with chronic disease risk. Traditional methods studying diseases and health outcomes do not typically look at the joint effect of exposures and biological mechanisms, overlooking complex interactions. This research can help inform precision public health strategies by identifying high risk population groups (expotypes) shaped by both lived experiences and biological processes, enabling more targeted prevention, early intervention, and equitable health policy development.

## Methods
- 28 exposure variables, 1343 protein levels, demographics (age, sex, ethnic background) of 41,804 UK Biobank participants
- Participants clustered based on their exposures with unsupervised gaussian mixture modeling resulting in seven expotypes
- Cluster description (separate analyses for exposures and proteins)
  - Univariate logistic regression
  - Stability selection LASSO to identify key exposures and proteins per cluster
  - Refit multivariable logistic regression models on the stably selected exposures/proteins to estimate effect sizes (odds ratios) to quantify each variable's contribution to distinguishing clusters
- Functional enrichment via Reactome to interpret biological pathways
- Disease analysis (Parkinson's Disease, Diabetes, Alzheimer's, CDK, CAD)
  - Calculate standardised disease incidence rates across clusters
  - Logistic regression models to assess predictive capability for chronic disease outcomes. Three sets of predictors were used: clusters alone, clusters + exposures, clusters + exposures + proteins

## Key findings
- 

## Repo Structure
- /Expotypes_Presentation.pdf: final presentation for project
- /Expotypes_Report.pdf: final report for project
- /Aggregations: aggregate categorical exposure variables
- /Missing: deal with missing data
- /Imputation: data imputation (MissRanger)
- /Preprocessing: merge protein and exposure data, remove outliers, scale and encode data
- /Descriptive_Statistics: table 1s, variable distributions
- /Dimensionality_Reduction: apply dimensionality reduction techniques to data (PCA, UMAP, t-SNE)
- /Clustering: apply different clustering techniques to exposure data (biclustering, fuzzy clustering, gaussian mixture model, hierarchical, hddc) and evaluate cluster performance
- /Analysis: exposure and protein univariate logistic regression, stability selection LASSO, and refit multivariable logistic regression models
- /Diseases_Analysis: standardised disease incidence rates, logistic regression models for chronic disease outcomes (predictors -> clusters alone, clusters + exposures, clusters + exposures + proteins)
- /Visualisations: various visuals from gaussian mixture model clustering, exposure analysis, and protein analysis
