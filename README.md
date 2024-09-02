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
Machine learning:
```Python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.metrics import  roc_curve, auc, classification_report, confusion_matrix, ConfusionMatrixDisplay, accuracy_score, roc_auc_score
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import GridSearchCV, StratifiedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
```

-------------------------------------

<b>Prospects:</b> 1. Incoporate explainable AI such as SHAP analysis for diagnosis or other binary classification problems in cancer microbiome; 2. Leverage more ML models or advanced ML models to the framework; 3. More data to be fed into the framework would be appreciating for more accurate microbial involvement identification for CRC.

<b>Tip:</b> Currently, we only have data from published paper or other cohort. Our own data is still generating in the lab.
