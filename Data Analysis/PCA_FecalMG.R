#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")

rm(list=ls())

library(ggplot2)
library(factoextra)
library(dplyr)

setwd("C:/MSc_Internship_Final/R_Scripts")

FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")

stage_mapping <- c(
  'Stage_0'= 'CRC',
  'Stage_I_II'= 'CRC',
  'Stage_III_IV'= 'CRC',
  'MP'= 'CRC',
  'HS'= 'CRC'
)

# Replacing the values in the 'Stage' column using the mapping
FMG_CRC_BA_HealCon <- FMG_CRC_BA %>%
  mutate(Group = recode(Group, !!!stage_mapping))

X <- FMG_CRC_BA_HealCon[, !names(FMG_CRC_BA_HealCon) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                            'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = FMG_CRC_BA_HealCon$Group, 
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  
             legend.title = "Group" 
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle("FMG_CRC vs. FMG_Healthy") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# For stage

stage_mapping <- c(
  'Stage_I_II'= 'Early Stage', 
  'Stage_III_IV'= 'Late Stage'
)

# Replacing the values in the 'Stage' column using the mapping
FMG_CRC_BA_stage <- FMG_CRC_BA %>%
  mutate(Group = recode(Group, !!!stage_mapping))

FMG_CRC_BA_stage <- FMG_CRC_BA_stage[FMG_CRC_BA_stage$Group != 'Healthy', ]

X <- FMG_CRC_BA_stage[, !names(FMG_CRC_BA_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                  'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor stage
fviz_pca_ind(pca_result,
             geom.ind = "point",  
             col.ind = FMG_CRC_BA_stage$Group,  
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38", 
                         "#6A3D9A"),
             addEllipses = TRUE, 
             legend.title = "Group"  
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle("FMG_Tumor_Stage")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

# For tumor location
TL_mapping <- c(
  'Left colon and Right colon'= 'Others',
  'Left colon and Rectum'= 'Others',
  'Rectum and Left colon'= 'Others',
  'Rectum, Left colon and Right colon'= 'Others'
)

# Replacing the values in the 'Stage' column using the mapping
FMG_CRC_BA_TL <- FMG_CRC_BA %>%
  mutate(Tumor.location = recode(Tumor.location, !!!TL_mapping))

FMG_CRC_BA_TL <- FMG_CRC_BA_TL[FMG_CRC_BA_TL$Tumor.location != '-', ]

X <- FMG_CRC_BA_TL[, !names(FMG_CRC_BA_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                  'Tumor.location')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for tumor location
fviz_pca_ind(pca_result,
             geom.ind = "point", 
             col.ind = FMG_CRC_BA_TL$Tumor.location,  
             palette = c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38"),
             addEllipses = TRUE,  
             legend.title = "Group" 
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle("FMG_Tumor_Location") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

