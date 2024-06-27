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
# PCA and TSNE for BLCA
rm(list=ls())
BLCA <- read.csv("Poore_BLCA")
BLCA <- unique(BLCA)
X_BLCA <- BLCA[, !names(BLCA) %in% c('sample_id1', 'gdc_file_uuid', 'filename', 'age_at_diagnosis', 'aliquot_uuid',
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
BLCA_scaled <- scale(X_BLCA)

# Perform PCA
pca_BLCA <- prcomp(BLCA_scaled, center = TRUE, scale. = TRUE)

# Extract the percentage of variance explained by PC1 and PC2
explained_var_BLCA <- pca_BLCA$sdev^2 / sum(pca_BLCA$sdev^2) * 100

# Visualize PCA for BLCA-sampletypes
fviz_pca_ind(pca_BLCA,
             geom.ind = "point",  # Show points only (no text)
             col.ind = BLCA$sample_type,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07", "#E7B800"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var_BLCA[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var_BLCA[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# TSNE based on PCA result

pca_data_BLCA <- pca_BLCA$x
pca_data_BLCA <- unique(pca_data_BLCA)

# Perform t-SNE on PCA results
tsne_result_BLCA <- Rtsne(pca_data_BLCA, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# Prepare t-SNE result for plotting
tsne_data_BLCA <- data.frame(tsne_result_BLCA$Y)

# Ensure both datasets have the same number of rows for alignment
n_samples <- length(BLCA$sample_type)
tsne_data_BLCA <- tsne_data_BLCA[1:n_samples, , drop = FALSE]
tsne_data_BLCA$Group <- BLCA$sample_type
colnames(tsne_data_BLCA) <- c("TSNE1", "TSNE2", "Group")

color_palette_BLCA <- c("#00AFBB", "#FC4E07", "#E7B800")

# Plot t-SNE result with ellipses
ggplot(tsne_data_BLCA, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_BLCA) +  # Set the same color palette
  scale_fill_manual(values = color_palette_BLCA) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

#------------
#------------
# PCA and TSNE for BRCA
rm(list=ls())
BRCA <- read.csv("Poore_BRCA")
BRCA <- unique(BRCA)
X_BRCA <- BRCA[, !names(BRCA) %in% c('sample_id1', 'gdc_file_uuid', 'filename', 'age_at_diagnosis', 'aliquot_uuid',
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
BRCA_scaled <- scale(X_BRCA)

# Perform PCA
pca_BRCA <- prcomp(BRCA_scaled, center = TRUE, scale. = TRUE)

# Extract the percentage of variance explained by PC1 and PC2
explained_var_BRCA <- pca_BRCA$sdev^2 / sum(pca_BRCA$sdev^2) * 100

# Visualize PCA for BRCA-sampletypes
fviz_pca_ind(pca_BRCA,
             geom.ind = "point",  # Show points only (no text)
             col.ind = BRCA$sample_type,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#6A3D9A"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var_BRCA[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var_BRCA[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# TSNE based on PCA result

pca_data_BRCA <- pca_BRCA$x
pca_data_BRCA <- unique(pca_data_BRCA)

# Perform t-SNE on PCA results
tsne_result_BRCA <- Rtsne(pca_data_BRCA, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# Prepare t-SNE result for plotting
tsne_data_BRCA <- data.frame(tsne_result_BRCA$Y)

# Ensure both datasets have the same number of rows for alignment
n_samples <- length(BRCA$sample_type)
tsne_data_BRCA <- tsne_data_BRCA[1:n_samples, , drop = FALSE]
tsne_data_BRCA$Group <- BRCA$sample_type
colnames(tsne_data_BRCA) <- c("TSNE1", "TSNE2", "Group")

color_palette_BRCA <- c("#00AFBB", "#FC4E07", "#E7B800", "#6A3D9A")

# Plot t-SNE result with ellipses
ggplot(tsne_data_BRCA, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_BRCA) +  # Set the same color palette
  scale_fill_manual(values = color_palette_BRCA) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

#------------
#------------
# PCA and TSNE for COAD
rm(list=ls())
COAD <- read.csv("Poore_COAD")
COAD <- unique(COAD)
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

# TSNE based on PCA result

pca_data_COAD <- pca_COAD$x
pca_data_COAD <- unique(pca_data_COAD)

# Perform t-SNE on PCA results
tsne_result_COAD <- Rtsne(pca_data_COAD, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# Prepare t-SNE result for plotting
tsne_data_COAD <- data.frame(tsne_result_COAD$Y)

# Ensure both datasets have the same number of rows for alignment
n_samples <- length(COAD$sample_type)
tsne_data_COAD <- tsne_data_COAD[1:n_samples, , drop = FALSE]
tsne_data_COAD$Group <- COAD$sample_type
colnames(tsne_data_COAD) <- c("TSNE1", "TSNE2", "Group")

color_palette_COAD <- c("#00AFBB", "#FC4E07", "#E7B800", "#6A3D9A", "#1F78B4")

# Plot t-SNE result with ellipses
ggplot(tsne_data_COAD, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_COAD) +  # Set the same color palette
  scale_fill_manual(values = color_palette_COAD) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

#------------
#------------
# PCA and TSNE for GBM
rm(list=ls())
GBM <- read.csv("Poore_GBM")
GBM <- unique(GBM)
X_GBM <- GBM[, !names(GBM) %in% c('sample_id1', 'gdc_file_uuid', 'filename', 'age_at_diagnosis', 'aliquot_uuid',
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
GBM_scaled <- scale(X_GBM)

# Perform PCA
pca_GBM <- prcomp(GBM_scaled, center = TRUE, scale. = TRUE)

# Extract the percentage of variance explained by PC1 and PC2
explained_var_GBM <- pca_GBM$sdev^2 / sum(pca_GBM$sdev^2) * 100

# Visualize PCA for BLCA-sampletypes
fviz_pca_ind(pca_GBM,
             geom.ind = "point",  # Show points only (no text)
             col.ind = GBM$sample_type,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07", "#E7B800"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var_GBM[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var_GBM[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# TSNE based on PCA result

pca_data_GBM <- pca_GBM$x
pca_data_GBM <- unique(pca_data_GBM)

# Perform t-SNE on PCA results
tsne_result_GBM <- Rtsne(pca_data_GBM, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# Prepare t-SNE result for plotting
tsne_data_GBM <- data.frame(tsne_result_GBM$Y)
tsne_data_GBM$Group <- GBM$sample_type
colnames(tsne_data_GBM) <- c("TSNE1", "TSNE2", "Group")

color_palette_GBM <- c("#00AFBB", "#FC4E07", "#E7B800")

# Plot t-SNE result with ellipses
ggplot(tsne_data_GBM, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_GBM) +  # Set the same color palette
  scale_fill_manual(values = color_palette_GBM) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)
