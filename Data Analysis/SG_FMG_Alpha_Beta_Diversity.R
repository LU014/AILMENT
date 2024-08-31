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

SG_FMG_BA <- read.csv("SG_FMG_HealCon.csv")

metadata <- SG_FMG_BA[, names(SG_FMG_BA) %in% c('Condition')]

SG_FMG_BA <- SG_FMG_BA[, !names(SG_FMG_BA) %in% c('Condition')]

shannon_diversity <- diversity(SG_FMG_BA, index = "shannon")

alpha_diversity <- data.frame(
  Sample = rownames(SG_FMG_BA),
  Shannon = shannon_diversity
)

alpha_diversity <- cbind(alpha_diversity, metadata)

color_palette <- "Set2"

# Shannon Diversity Boxplot

pairwise_wilcox <- pairwise.wilcox.test(alpha_diversity$Shannon, alpha_diversity$metadata,
                                        p.adjust.method = "BH")
ggplot(alpha_diversity, aes(x = metadata, y = Shannon, fill = metadata)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = list(c("CRC", "Healthy")),  # Replace with actual group names
              map_signif_level = TRUE, test = "wilcox.test") +  # Automatically add significance levels
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(title = "Healthy Condition", x = "", y = "") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 13),
    legend.position = "none"
  ) +
  coord_fixed(ratio = 0.9)

## Beta Diversity

# Bray-Curtis PCoA

#--

bray_curtis <- vegdist(SG_FMG_BA, method = "bray")
pcoa_bray <- cmdscale(bray_curtis, eig = TRUE, k = 2)
pcoa_bray_df <- data.frame(Sample = rownames(SG_FMG_BA), 
                           Axis1 = pcoa_bray$points[,1], 
                           Axis2 = pcoa_bray$points[,2])
# Merge with metadata
pcoa_bray_df <- cbind(pcoa_bray_df, metadata)

eigenvalues <- pcoa_bray$eig
total_variance <- sum(eigenvalues)
percent_variance <- 100 * eigenvalues[1:2] / total_variance
axis1_label <- paste0("PCoA 1 (", round(percent_variance[1], 2), "%)")
axis2_label <- paste0("PCoA 2 (", round(percent_variance[2], 2), "%)")

permanova_result <- adonis2(bray_curtis ~ metadata, permutations = 999)
p_value <- permanova_result$`Pr(>F)`[1]
ggplot(pcoa_bray_df, aes(x = Axis1, y = Axis2, color = metadata, shape = metadata)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(fill = metadata), geom = "polygon", alpha = 0.2, level = 0.95) + # Add ellipses
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



