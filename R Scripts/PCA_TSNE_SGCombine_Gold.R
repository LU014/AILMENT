#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("dplyr")

rm(list=ls())

library(ggplot2)
library(factoextra)
library(dplyr)

setwd("C:/MSc_Internship/R_Scripts")

SG_Gold_CRC_combined <- read.csv("SingaporeCRC_data/SG_Gold_CRC_combined")
X <- SG_Gold_CRC_combined[, !names(SG_Gold_CRC_combined) %in% c('patient_id', 'TMB', 'KRAS', 'BRAF', 'NRAS', 'TP53', 'APC', 'PIK3CA',
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

# Visualize PCA with factoextra
fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_Gold_CRC_combined$iCMS,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# TSNE based on PCA result

#install.packages("Rtsne")
library(Rtsne)

pca_data <- pca_result$x

# Perform t-SNE on PCA results
tsne_result <- Rtsne(pca_data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# Prepare t-SNE result for plotting
tsne_data <- data.frame(tsne_result$Y)
tsne_data$Group <- SG_Gold_CRC_combined$iCMS# Assuming 'MSI.Status' is your grouping variable
colnames(tsne_data) <- c("TSNE1", "TSNE2", "Group")

color_palette_iCMS <- c("#00AFBB", "#FC4E07")

# Plot t-SNE result with ellipses
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_iCMS) +  # Set the same color palette
  scale_fill_manual(values = color_palette_iCMS) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

# for MSI status
color_palette_MSI <- c("#00AFBB", "#FC4E07")

fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_Gold_CRC_combined$MSI.Status,  # Color by groups (replace "Group" with your column name)
             palette = color_palette_MSI,
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# Prepare t-SNE result for plotting
tsne_data <- data.frame(tsne_result$Y)
tsne_data$Group <- SG_Gold_CRC_combined$MSI.Status  # Assuming 'MSI.Status' is your grouping variable
colnames(tsne_data) <- c("TSNE1", "TSNE2", "Group")

# Plot t-SNE result with ellipses
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_MSI) +  # Set the same color palette
  scale_fill_manual(values = color_palette_MSI) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

# Visualize PCA for TumorSites
color_palette_TS <- c("#00AFBB", "#FC4E07", "#E7B800", "#00BA38", 
                   "#6A3D9A", "#FF7F00", "#B15928", "#1F78B4", "#D81B60")

fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_Gold_CRC_combined$Site.of.Primary.Colorectal.tumour,  # Color by groups (replace "Group" with your column name)
             palette = color_palette_TS,
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# Prepare t-SNE result for plotting
tsne_data <- data.frame(tsne_result$Y)
tsne_data$Group <- SG_Gold_CRC_combined$Site.of.Primary.Colorectal.tumour  # Assuming 'MSI.Status' is your grouping variable
colnames(tsne_data) <- c("TSNE1", "TSNE2", "Group")

# Plot t-SNE result with ellipses
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size=1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_TS) +  # Set the same color palette
  scale_fill_manual(values = color_palette_TS) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)

# Visualize PCA for Vital Status
fviz_pca_ind(pca_result,
             geom.ind = "point",  # Show points only (no text)
             col.ind = SG_Gold_CRC_combined$Vital.status,  # Color by groups (replace "Group" with your column name)
             palette = c("#00AFBB", "#FC4E07"),
             addEllipses = TRUE,  # Add concentration ellipses
             legend.title = "Group"  # Adjust the legend title
) +
  labs(x = paste0("PC1 (", round(explained_var[1], 2), "%)"),
       y = paste0("PC2 (", round(explained_var[2], 2), "%)")) +
  ggtitle(NULL) +  # Remove the title
  theme_minimal()

# Prepare t-SNE result for plotting
tsne_data <- data.frame(tsne_result$Y)
tsne_data$Group <- SG_Gold_CRC_combined$Vital.status  # Assuming 'MSI.Status' is your grouping variable
colnames(tsne_data) <- c("TSNE1", "TSNE2", "Group")

color_palette_VS <- c("#00AFBB", "#FC4E07")

# Plot t-SNE result with ellipses
ggplot(tsne_data, aes(x = TSNE1, y = TSNE2, color = Group, fill = Group)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2, size = 1.2, show.legend = FALSE) +  # Add concentration ellipses
  scale_color_manual(values = color_palette_VS) +  # Set the same color palette
  scale_fill_manual(values = color_palette_VS) +   # Set the same fill palette for ellipses
  theme_minimal() +
  labs(color = "Group", fill = "Group") +
  ggtitle(NULL)
