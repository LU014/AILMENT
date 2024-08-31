#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")

rm(list=ls())

library(ggplot2)
library(factoextra)
library(dplyr)

setwd("C:/MSc_Internship_Final/R_Scripts")

SG_FMG_BA <- read.csv("SG_FMG_HealCon.csv")


X <- SG_FMG_BA[, !names(SG_FMG_BA) %in% c('Condition')]

X <- X[, sapply(X, function(col) sd(col) != 0)]

# Standardize the numeric data
X_scaled <- scale(X)

pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100

# Visualize PCA for Healthy Condition
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = SG_FMG_BA$Condition, 
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  
             legend.title = "Group" 
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle("Healthy Condition") +  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title



