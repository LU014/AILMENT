# README

## EMC_CanML: A Novel Machine Learning Framework for Prediction and Analysis of Microbial Involvement in Colorectal Cancer

### Workflow for you to conduct EMC_CanML step by step:

1. Install Requirements

Data analysis in R (PCA, alpha diversity, beta diversity):
```bash
install.packages("ggplot2")
install.packages("factoextra")
install.packages("dplyr")
install.packages("vegan")
```
Ensembl_id annotation in R:
```bash
install.packages("BiocManager")  # BiocManager is required to install Bioconductor packages
BiocManager::install("biomaRt")
install.packages("dplyr")
```
Machine learning in Python:
```bash
pip install pandas numpy matplotlib seaborn scikit-learn imbalanced-learn
```
2. Prepare your dataset (normalization and clean)

```Python
import pandas as pd

# Assuming your "data" is rows with samples and columns with taxa at the species level
data = pd.read_csv("data.tsv", index_col=0, sep = '\t') # or CSV file

data = data[data.index.str.startswith('k__Bacteria')] # Select bacteria kingdom

# Normalization
row_sums = data.sum(axis=1)
data = data.div(row_sums, axis=0)

# Data clean (keep only the species that present in more than 50% of samples)
non_zero_counts = (data > 0).sum(axis=0)
half_samples = len(data) / 2
data = data.loc[:, non_zero_counts > half_samples]

# Remove the invalid species or genera (e.g., the blacklist mentioned in our study)
invalid_names =['f__; g__; s__','g__; s__', ...] # here also remove the empty (or un-identified taxa)
for col in data.columns:
    if any(invalid_name in col for invalid_name in invalid_names):
        data.drop(col, axis=1, inplace=True)

# Extract only the genus level taxonomy
def extract_taxonomy(column):
    taxonomy_levels = [t for t in column.split('; ') if t.startswith('g__')]
    return '; '.join(taxonomy_levels) if taxonomy_levels else column
new_columns = [extract_taxonomy(column) for column in data.columns]
data.columns = new_columns

# Summing species to genus
data = data.groupby(data.columns, axis=1).sum()

# Together with metadata ('patient_id' can be other common parts from your data)
data = pd.merge(data, metadata, on='patient_id', how='inner') 
```
3. Machine learning models/framework

For conducting ML models for EMC_CanML, you can follow the code in Examples, where you will see examples for RF or XGB with binary classification or multi-class classification.

4. Integrative evaluation of predictive performance



-------------------------------------

<b>Prospects:</b> 1. Incoporate explainable AI such as SHAP analysis for diagnosis or other binary classification problems in cancer microbiome; 2. Leverage more ML models or advanced ML models to the framework; 3. More data to be fed into the framework would be appreciating for more accurate microbial involvement identification for CRC.

<b>Tip:</b> Currently, we only have data from published paper or other cohort. Our own data is still generating in the lab.
