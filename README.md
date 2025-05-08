# Expotype-Clustering
## Overview
This project identifies exposure-driven clusters (expotypes) in UK Biobank participants, characterises their proteomic signatures, and examines associations with chronic disease risk. Traditional methods of studying diseases and health outcomes do not typically look at the joint effect of exposures and biological mechanisms, overlooking complex interactions. This research can help inform precision public health strategies by identifying high-risk population groups (expotypes) shaped by both lived experiences and biological processes, enabling more targeted prevention, early intervention, and equitable health policy development.

## Methods
- 28 exposure variables, 1343 protein levels, demographics (age, sex, ethnic background) of 41,804 UK Biobank participants
- Participants clustered based on their exposures with unsupervised Gaussian mixture modelling, resulting in seven expotypes
- Cluster description (separate analyses for exposures and proteins):
  - Univariate logistic regression
  - Stability selection LASSO to identify key exposures and proteins per cluster
  - Refit multivariable logistic regression models on the stably selected exposures/proteins to estimate effect sizes (odds ratios) and quantify each variable's contribution to distinguishing clusters
- Functional enrichment via Reactome to interpret biological pathways for clusters
- Disease analysis (Alzheimer's, Parkinson's, CKD, CAD, diabetes)
  - Calculate standardised disease incidence rates across clusters
  - Logistic regression models to assess predictive capability for chronic disease outcomes. Three sets of predictors were used: clusters alone, clusters + exposures, clusters + exposures + proteins

## Key findings
- Cluster 4, characterised by heavy smoking, showed the highest incidence of Alzheimer's, Parkinson's, CKD, and CAD and was linked to disrupted brain signalling pathways (e.g. NTRK3). Cluster 5, characterised by low smoking, low anxiety, and low deprivation, had reduced odds of CKD, CAD, and diabetes and was associated with protective immune signaling (e.g. IL-33). This highlights the strong influence of behavioural, social, and molecular factors jointly shaping chronic disease risk.
- Disease prediction models improved with proteomic data, particularly for CKD, suggesting that proteins contain information not captured by clusters or exposures alone. This highlights the importance of incorporating protein data in disease prediction models and indicates that not all relevant information can be fully encoded through exposure variables.

## Repo Structure
- /Expotypes_Presentation.pdf: Final presentation for project
- /Expotypes_Report.pdf: Final report for project
- /Aggregations: Aggregate categorical exposure variables
- /Missing: Handle with missing data
- /Imputation: Data imputation (missRanger)
- /Preprocessing: Merge protein and exposure data, remove outliers, scale and encode data
- /Descriptive_Statistics: Table 1s, variable distributions
- /Dimensionality_Reduction: Apply dimensionality reduction techniques to exposure data (PCA, UMAP, t-SNE)
- /Clustering: Apply clustering techniques to exposure data (biclustering, fuzzy clustering, Gaussian mixture model, hierarchical, HDDC) and evaluate cluster performance
- /Analysis: Exposure and protein univariate logistic regression, stability selection LASSO, and refit multivariable logistic regression models
- /Diseases_Analysis: Standardised disease incidence rates, logistic regression models for chronic disease outcomes
- /Visualisations: Various visuals from Gaussian mixture model clustering, exposure analysis, and protein analysis
