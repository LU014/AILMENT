#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")
#install.packages("Rtsne")

library(ggplot2)
library(factoextra)
library(dplyr)
library(Rtsne)

setwd("C:/MSc_Internship/R_Scripts")

#------------
#------------
# PCA for COAD
rm(list=ls())
COAD <- read.csv("Poore_COAD")
COAD <- COAD %>%
  filter(sample_type != 'Metastatic' & sample_type != 'Recurrent Tumor')
X_COAD <- COAD[, !names(COAD) %in% c('sample_id1', 'gdc_file_uuid', 'filename', 'age_at_diagnosis', 'aliquot_uuid',
                                     'case_uuid', 'days_to_death', 'disease_type', 'ethnicity',
                                     'experimental_strategy', 'gender', 'investigation', 'platform',
                                     'primary_site', 'race', 'reference_genome', 'sample_type',
                                     'sample_uuid', 'vital_status', 'tissue_source_site_label',
                                     'data_submitting_center_label', 'country_of_sample_procurement',
                                     'histological_diagnosis_label', 'pathologic_t_label',
                                     'pathologic_n_label', 'pathologic_stage_label', 'icd03_histology_label',
                                     'icd03_histology_site', 'icd10', 'portion_is_ffpe',
                                     'new_tumor_event_after_initial_trtmt',
                                     'primary_therapy_outcome_success_label', 'portion_weight',
                                     'aliquot_concentration', 'analyte_A260A280Ratio', 'analyte_amount',
                                     'analyte_type_label', 'radiation_therapy_code_label',
                                     'radiation_therapy_site_label', 'radiation_therapy_type_label',
                                     'year_of_diagnosis', 'vital_status_label')]


# Standardize the numeric data
COAD_scaled <- scale(X_COAD)

# Perform PCA
pca_COAD <- prcomp(COAD_scaled, center = TRUE, scale. = TRUE)

# Extract the percentage of variance explained by PC1 and PC2
explained_var_COAD <- pca_COAD$sdev^2 / sum(pca_COAD$sdev^2) * 100

# Visualize PCA for BRCA-sampletypes
fviz_pca_ind(pca_COAD,
             geom.ind = "point",  # Show points only (no text)
             col.ind = COAD$sample_type,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#6A3D9A", "#1F78B4"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var_COAD[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var_COAD[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()


