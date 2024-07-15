#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")

rm(list=ls())

library(ggplot2)
library(factoextra)
library(dplyr)

setwd("C:/MSc_Internship/R_Scripts")

SG_CL_CRC_BA <- read.csv("SingaporeCRC_data/SG_CL_CRC_BA")
stage_mapping <- c(
  'I' = 'Early Stage',
  'II' = 'Early Stage',
  'III' = 'Late Stage',
  'IV' = 'Late Stage',
  'IIA' = 'Early Stage',
  'IIB' = 'Early Stage',
  'IIC' = 'Early Stage',
  'IIIA' = 'Late Stage',
  'IIIB' = 'Late Stage',
  'IIIC' = 'Late Stage',
  'IVA' = 'Late Stage',
  'IVB' = 'Late Stage'
)

# Replacing the values in the 'Stage' column using the mapping
SG_CL_CRC_BA <- SG_CL_CRC_BA %>%
  mutate(Stage = recode(Stage, !!!stage_mapping))

X <- SG_CL_CRC_BA[, !names(SG_CL_CRC_BA) %in% c('patient_id', 'TMB', 'KRAS', 'BRAF', 'NRAS', 'TP53', 'APC', 'PIK3CA',
                                          'PIK3R1', 'SMAD4', 'ERBB4', 'RNF43', 'ZNRF3', 'KIT', 'TGFBR2',
                                          'Vital.status', 'MSI.Status', 'CRIS', 'Gender', 'Age.at.Diagnosis',
                                          'Site.of.Primary.Colorectal.tumour', 'Side', 'Grade', 'TNM', 'Stage',
                                          'iCMS', 'CMS', 'group3', 'group5')]

# Identify and remove constant columns (columns with zero variance)
X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

# Perform PCA
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Extract the percentage of variance explained by PC1 and PC2
explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_CL_CRC_BA$Stage,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()


# for MSI status
color_palette_MSI <- c("#00AFBB", "#FC4E07")

fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_CL_CRC_BA$MSI.Status,  # Color by groups (replace "Group" with your column name)
             palette = color_palette_MSI,
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()


# Visualize PCA for TumorSites
color_palette_TS <- c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38", 
                   "#6A3D9A", "#FF7F00", "#B15928", "#1F78B4", "#D81B60")

fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_CL_CRC_BA$Site.of.Primary.Colorectal.tumour,  # Color by groups (replace "Group" with your column name)
             palette = color_palette_TS,
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

