#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")

rm(list=ls())

library(ggplot2)
library(factoextra)
library(dplyr)

setwd("C:/MSc_Internship/R_Scripts")

FecalMG_group <- read.csv("FecalMG_Group")

X <- FecalMG_group[, !names(FecalMG_group) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                            'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = FecalMG_group$Group, 
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  
             legend.title = "Group" 
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# For stage
FecalMG_stage <- read.csv("FecalMG_Stage")

X <- FecalMG_stage[, !names(FecalMG_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                  'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point",  
             col.ind = FecalMG_stage$Group,  
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38", 
                         "#6A3D9A"),
             addEllipses = TRUE, 
             legend.title = "Group"  
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# For tumor location
FecalMG_TL <- read.csv("FecalMG_TL")

X <- FecalMG_TL[, !names(FecalMG_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                  'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point", 
             col.ind = FecalMG_TL$Tumor.location,  
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38"),
             addEllipses = TRUE,  
             legend.title = "Group" 
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

