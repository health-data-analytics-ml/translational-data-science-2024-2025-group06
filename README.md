# Expotype Clustering
## Overview
This project identifies exposure-driven clusters (expotypes) in 41,804 UK Biobank participants, characterises their proteomic signatures, and examines associations with chronic disease risk. Traditional methods studying diseases and health outcomes do not look at the joint effect of exposures and biological mechanisms typically, overlooking complex interactions. We aimed to capture the complex, real world conditions that shape health.

## Methods


## Key findings


## Repo Structure
- /Expotypes_Presentation.pdf: final presentation for project
- /Expotypes_Report.pdf: final report for project
- /Aggregations: aggregate categorical exposure variables
- /Missing: deal with missing data
- /Imputation: data imputation
- /Preprocessing: merge protein and exposure data, remove outliers, scale and encode data
- /Descriptive_Statistics: table 1s, variable distributions
- /Dimensionality_Reduction: apply dimensionality reduction techniques to data (PCA, UMAP, t-SNE)
- /Clustering: apply different clustering techniques to exposure data (biclustering, fuzzy clustering, gaussian mixture model, hierarchical, hddc) and evaluate cluster performance
- /Analysis: exposure and protein univariate analysis, stability selection LASSO, and logistic regression models to characterise the clusters through exposures and proteomic signatures
- /Diseases_Analysis: evaluate how disease risk varies across clusters and predicts disease, compare models based on clusters alone, clusters + exposures, and clusters + exposures + proteins
- /Visualisations: various visuals from gaussian mixture model clustering, exposure analysis, and protein analysis
