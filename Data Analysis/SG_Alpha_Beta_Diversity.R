#install.packages("vegan")

library(vegan)
library(ggplot2)
library(RColorBrewer)
library(ggsignif)
library(ggpubr)
library(RColorBrewer)

rm(list=ls())

setwd("C:/MSc_Internship_Final/R_Scripts")

SG_CRC_BA <- read.csv("SG_CRC_BA.csv")
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
SG_CRC_BA <- SG_CRC_BA %>%
  mutate(Stage = recode(Stage, !!!stage_mapping))

metadata <- SG_CRC_BA[, names(SG_CRC_BA) %in% c('TMB', 'KRAS', 'BRAF', 'NRAS', 'TP53', 'APC', 'PIK3CA',
                                                 'PIK3R1', 'SMAD4', 'ERBB4', 'RNF43', 'ZNRF3', 'KIT', 'TGFBR2',
                                                 'Vital.status', 'MSI.Status', 'CRIS', 'Gender', 'Age.at.Diagnosis',
                                                 'Site.of.Primary.Colorectal.tumour', 'Side', 'Grade', 'TNM', 'Stage',
                                                 'iCMS', 'CMS', 'group3', 'group5')]

SG_CRC_BA <- SG_CRC_BA[, !names(SG_CRC_BA) %in% c('patient_id', 'TMB', 'KRAS', 'BRAF', 'NRAS', 'TP53', 'APC', 'PIK3CA',
                                          'PIK3R1', 'SMAD4', 'ERBB4', 'RNF43', 'ZNRF3', 'KIT', 'TGFBR2',
                                          'Vital.status', 'MSI.Status', 'CRIS', 'Gender', 'Age.at.Diagnosis',
                                          'Site.of.Primary.Colorectal.tumour', 'Side', 'Grade', 'TNM', 'Stage',
                                          'iCMS', 'CMS', 'group3', 'group5')]

shannon_diversity <- diversity(SG_CRC_BA, index = "shannon")

alpha_diversity <- data.frame(
  Sample = rownames(SG_CRC_BA),
  Shannon = shannon_diversity
)

alpha_diversity <- cbind(alpha_diversity,metadata)

color_palette <- "Set2"

# Shannon Diversity Boxplot

pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$BRAF,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = BRAF, y = Shannon, fill = BRAF)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("mut", "wt")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "BRAF", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)
#---
pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$MSI.Status,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = MSI.Status, y = Shannon, fill = MSI.Status)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("MSI", "MSS")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "MSI Status", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)
#---
pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$TP53,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = TP53, y = Shannon, fill = TP53)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("mut", "wt")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "TP53", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)
#---
pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$Stage,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = Stage, y = Shannon, fill = Stage)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("Early Stage", "Late Stage")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "Tumor Stage", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)
#---
pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$Site.of.Primary.Colorectal.tumour,
                                        p.adjust.method = "BH")
comparisons <- list(
  c("Cecum", "Descending colon"),
  c("Rectosigmoid junction", "Rectum"),
  c("Descending colon", "Rectosigmoid junction")
)

ggplot(alpha_diversity, aes(x = Site.of.Primary.Colorectal.tumour, y = Shannon, fill = Site.of.Primary.Colorectal.tumour)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons, 
              map_signif_level = TRUE, 
              test = "wilcox.test", 
              textsize = 3) +  # Adjust text size if needed
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink", "cyan", "magenta", "yellow")) +
  theme_classic() +
  labs(title = "Tumor Location", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 1.2)

## Beta Diversity

# Bray-Curtis PCoA

bray_curtis <- vegdist(SG_CRC_BA, method = "bray")
pcoa_bray <- cmdscale(bray_curtis, eig = TRUE, k = 2)
pcoa_bray_df <- data.frame(Sample = rownames(SG_CRC_BA), 
                           Axis1 = pcoa_bray$points[,1], 
                           Axis2 = pcoa_bray$points[,2])
# Merge with metadata
pcoa_bray_df <- cbind(pcoa_bray_df, metadata)

eigenvalues <- pcoa_bray$eig
total_variance <- sum(eigenvalues)
percent_variance <- 100 * eigenvalues[1:2] / total_variance
axis1_label <- paste0("PCoA 1 (", round(percent_variance[1], 2), "%)")
axis2_label <- paste0("PCoA 2 (", round(percent_variance[2], 2), "%)")

#--
permanova_result <- adonis2(bray_curtis ~ metadata$BRAF, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = BRAF, shape = BRAF)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = BRAF), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "BRAF",
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
permanova_result <- adonis2(bray_curtis ~ metadata$MSI.Status, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = MSI.Status, shape = MSI.Status)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = MSI.Status), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "MSI Status",
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
permanova_result <- adonis2(bray_curtis ~ metadata$TP53, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = TP53, shape = TP53)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = TP53), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
  scale_color_brewer(palette = color_palette) +
  scale_fill_brewer(palette = color_palette, guide = "none") +
  theme_classic() +
  labs(
    title = "TP53",
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
permanova_result <- adonis2(bray_curtis ~ metadata$Stage, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = Stage, shape = Stage)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Stage), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
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
    plot.title = element_text(hjust = 0.5,  size = 16),
    axis.title = element_text( size = 13),
    axis.text = element_text(size = 13),
    legend.position = "right",
    legend.text = element_text(size = 13)
  )+
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", format(p_value, scientific = TRUE)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "black", fontface = "italic")

#--
permanova_result <- adonis2(bray_curtis ~ metadata$Site.of.Primary.Colorectal.tumour, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = Site.of.Primary.Colorectal.tumour, shape = Site.of.Primary.Colorectal.tumour)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = Site.of.Primary.Colorectal.tumour), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
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
