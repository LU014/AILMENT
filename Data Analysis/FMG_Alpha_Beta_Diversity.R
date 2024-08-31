#install.packages("vegan")

library(vegan)
library(ggplot2)
library(RColorBrewer)
library(ggsignif)
library(ggpubr)
library(RColorBrewer)
library(factoextra)
library(dplyr)

rm(list=ls())

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

metadata <- FMG_CRC_BA_HealCon[, names(FMG_CRC_BA_HealCon) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                  'Tumor.location')]

FMG_CRC_BA_HealCon <- FMG_CRC_BA_HealCon[, !names(FMG_CRC_BA_HealCon) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                             'Tumor.location')]

shannon_diversity <- diversity(FMG_CRC_BA_HealCon, index = "shannon")

alpha_diversity <- data.frame(
  Sample = rownames(FMG_CRC_BA_HealCon),
  Shannon = shannon_diversity
)

alpha_diversity <- cbind(alpha_diversity,metadata)

color_palette <- "Set2"

# Shannon Diversity Boxplot

pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$Group,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = Group, y = Shannon, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("CRC", "Healthy")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "Healthy Condition", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5,  size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)

#---
FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")
stage_mapping <- c(
  'Stage_I_II'= 'Early Stage', 
  'Stage_III_IV'= 'Late Stage'
)

# Replacing the values in the 'Stage' column using the mapping
FMG_CRC_BA_stage <- FMG_CRC_BA %>%
  mutate(Group = recode(Group, !!!stage_mapping))

FMG_CRC_BA_stage <- FMG_CRC_BA_stage[FMG_CRC_BA_stage$Group != 'Healthy', ]

metadata <- FMG_CRC_BA_stage[, names(FMG_CRC_BA_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                  'Tumor.location')]

FMG_CRC_BA_stage <- FMG_CRC_BA_stage[, !names(FMG_CRC_BA_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                             'Tumor.location')]

shannon_diversity <- diversity(FMG_CRC_BA_stage, index = "shannon")

alpha_diversity <- data.frame(
  Sample = rownames(FMG_CRC_BA_stage),
  Shannon = shannon_diversity
)

alpha_diversity <- cbind(alpha_diversity,metadata)

pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$Group,
                                        p.adjust.method = "BH")
comparisons <- list(
  c("Early Stage", "Late Stage")
)
ggplot(alpha_diversity, aes(x = Group, y = Shannon, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons,  
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "magenta", "yellow")) +
  theme_classic() +
  labs(title = "Tumor Stage", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5,  size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 1.2)
#---
FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")
TL_mapping <- c(
  'Left colon and Right colon'= 'Others',
  'Left colon and Rectum'= 'Others',
  'Rectum and Left colon'= 'Others',
  'Rectum, Left colon and Right colon'= 'Others'
)

# Replacing the values in the 'TL' column using the mapping
FMG_CRC_BA_TL <- FMG_CRC_BA %>%
  mutate(Tumor.location = recode(Tumor.location, !!!TL_mapping))

FMG_CRC_BA_TL <- FMG_CRC_BA_TL[FMG_CRC_BA_TL$Tumor.location != '-', ]

metadata <- FMG_CRC_BA_TL[, names(FMG_CRC_BA_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                              'Tumor.location')]

FMG_CRC_BA_TL <- FMG_CRC_BA_TL[, !names(FMG_CRC_BA_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                       'Tumor.location')]

shannon_diversity <- diversity(FMG_CRC_BA_TL, index = "shannon")

alpha_diversity <- data.frame(
  Sample = rownames(FMG_CRC_BA_TL),
  Shannon = shannon_diversity
)

alpha_diversity <- cbind(alpha_diversity,metadata)

pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$Tumor.location,
                                        p.adjust.method = "BH")
comparisons <- list(
  c("Left colon", "Others"),
  c("Others", "Rectum")
)
ggplot(alpha_diversity, aes(x = Tumor.location, y = Shannon, fill = Tumor.location)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons,  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "magenta", "yellow")) +
  theme_classic() +
  labs(title = "Tumor Location", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 1.2)

## Beta Diversity

# Bray-Curtis PCoA

#--
FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")
stage_mapping <- c(
  'Stage_0'= 'CRC',
  'Stage_I_II'= 'CRC',
  'Stage_III_IV'= 'CRC',
  'MP'= 'CRC',
  'HS'= 'CRC'
)
# Replacing the values in the 'Group' column using the mapping
FMG_CRC_BA_HealCon <- FMG_CRC_BA %>%
  mutate(Group = recode(Group, !!!stage_mapping))
metadata <- FMG_CRC_BA_HealCon[, names(FMG_CRC_BA_HealCon) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                  'Tumor.location')]
FMG_CRC_BA_HealCon <- FMG_CRC_BA_HealCon[, !names(FMG_CRC_BA_HealCon) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                             'Tumor.location')]
bray_curtis <- vegdist(FMG_CRC_BA_HealCon, method = "bray")
pcoa_bray <- cmdscale(bray_curtis, eig = TRUE, k = 2)
pcoa_bray_df <- data.frame(Sample = rownames(FMG_CRC_BA_HealCon), 
                           Axis1 = pcoa_bray$points[,1], 
                           Axis2 = pcoa_bray$points[,2])
# Merge with metadata
pcoa_bray_df <- cbind(pcoa_bray_df, metadata)

eigenvalues <- pcoa_bray$eig
total_variance <- sum(eigenvalues)
percent_variance <- 100 * eigenvalues[1:2] / total_variance
axis1_label <- paste0("PCoA 1 (", round(percent_variance[1], 2), "%)")
axis2_label <- paste0("PCoA 2 (", round(percent_variance[2], 2), "%)")

permanova_result <- adonis2(bray_curtis ~ metadata$Group, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = Group, shape = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Group), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "Healthy Condition",
    x = axis1_label,
    y = axis2_label,
    color = "Group",  # Title for the color legend
    shape = "Group"   # Title for the shape legend
  ) +
  theme(
    plot.title = element_text(hjust = 0.5,  size = 16),
    axis.title = element_text( size = 13),
    axis.text = element_text(size = 13),
    legend.position = "right",
    legend.text = element_text(size = 13)
  )+
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", format(p_value, scientific = TRUE)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black", fontface = "italic")

#--
FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")
stage_mapping <- c(
  'Stage_I_II'= 'Early Stage', 
  'Stage_III_IV'= 'Late Stage'
)

# Replacing the values in the 'Stage' column using the mapping
FMG_CRC_BA_stage <- FMG_CRC_BA %>%
  mutate(Group = recode(Group, !!!stage_mapping))

FMG_CRC_BA_stage <- FMG_CRC_BA_stage[FMG_CRC_BA_stage$Group != 'Healthy', ]

metadata <- FMG_CRC_BA_stage[, names(FMG_CRC_BA_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                              'Tumor.location')]

FMG_CRC_BA_stage <- FMG_CRC_BA_stage[, !names(FMG_CRC_BA_stage) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                                       'Tumor.location')]
bray_curtis <- vegdist(FMG_CRC_BA_stage, method = "bray")
pcoa_bray <- cmdscale(bray_curtis, eig = TRUE, k = 2)
pcoa_bray_df <- data.frame(Sample = rownames(FMG_CRC_BA_stage), 
                           Axis1 = pcoa_bray$points[,1], 
                           Axis2 = pcoa_bray$points[,2])
# Merge with metadata
pcoa_bray_df <- cbind(pcoa_bray_df, metadata)

eigenvalues <- pcoa_bray$eig
total_variance <- sum(eigenvalues)
percent_variance <- 100 * eigenvalues[1:2] / total_variance
axis1_label <- paste0("PCoA 1 (", round(percent_variance[1], 2), "%)")
axis2_label <- paste0("PCoA 2 (", round(percent_variance[2], 2), "%)")

permanova_result <- adonis2(bray_curtis ~ metadata$Group, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = Group, shape = Group)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Group), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "Tumor Stage",
    x = axis1_label,
    y = axis2_label,
    color = "Group",  # Title for the color legend
    shape = "Group"   # Title for the shape legend
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text( size = 13),
    axis.text = element_text(size = 13),
    legend.position = "right",
    legend.text = element_text(size = 13)
  )+
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", format(p_value, scientific = TRUE)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black", fontface = "italic")

#--
FMG_CRC_BA <- read.csv("FMG_CRC_BA.csv")
TL_mapping <- c(
  'Left colon and Right colon'= 'Others',
  'Left colon and Rectum'= 'Others',
  'Rectum and Left colon'= 'Others',
  'Rectum, Left colon and Right colon'= 'Others'
)

# Replacing the values in the 'TL' column using the mapping
FMG_CRC_BA_TL <- FMG_CRC_BA %>%
  mutate(Tumor.location = recode(Tumor.location, !!!TL_mapping))

FMG_CRC_BA_TL <- FMG_CRC_BA_TL[FMG_CRC_BA_TL$Tumor.location != '-', ]

metadata <- FMG_CRC_BA_TL[, names(FMG_CRC_BA_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                        'Tumor.location')]

FMG_CRC_BA_TL <- FMG_CRC_BA_TL[, !names(FMG_CRC_BA_TL) %in% c('Group', 'Stage', 'Age', 'Gender', 'BMI', 'Brinkman.Index', 'Alcohol',
                                                              'Tumor.location')]
bray_curtis <- vegdist(FMG_CRC_BA_TL, method = "bray")
pcoa_bray <- cmdscale(bray_curtis, eig = TRUE, k = 2)
pcoa_bray_df <- data.frame(Sample = rownames(FMG_CRC_BA_TL), 
                           Axis1 = pcoa_bray$points[,1], 
                           Axis2 = pcoa_bray$points[,2])
# Merge with metadata
pcoa_bray_df <- cbind(pcoa_bray_df, metadata)

eigenvalues <- pcoa_bray$eig
total_variance <- sum(eigenvalues)
percent_variance <- 100 * eigenvalues[1:2] / total_variance
axis1_label <- paste0("PCoA 1 (", round(percent_variance[1], 2), "%)")
axis2_label <- paste0("PCoA 2 (", round(percent_variance[2], 2), "%)")

permanova_result <- adonis2(bray_curtis ~ metadata$Tumor.location, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = Tumor.location, shape = Tumor.location)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Tumor.location), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "Tumor Location",
    x = axis1_label,
    y = axis2_label,
    color = "Group",  # Title for the color legend
    shape = "Group"   # Title for the shape legend
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text( size = 13),
    axis.text = element_text(size = 13),
    legend.position = "right",
    legend.text = element_text(size = 13)
  )+
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", format(p_value, scientific = TRUE)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black", fontface = "italic")


