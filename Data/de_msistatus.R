
library(dplyr)
setwd("C:/MSc_Internship/R_Scripts")
load('filtered_SG_GE.Rdata') # filtered (cleaned) human gene expression data
metadata=read.csv('SGCRCdata/SG-BULK_patient_clinical_information.csv',header=T)

metadata$patient_id=paste0("X", metadata$patient_id)
filtered_metadata <- metadata %>% filter(patient_id %in% colnames(eset1) & MSI.Status %in% c('MSS', 'MSI'))
rownames(filtered_metadata)=filtered_metadata$patient_id
counts=eset1[,colnames(eset1) %in% rownames(filtered_metadata)]
gl=filtered_metadata[match(colnames(counts),rownames(filtered_metadata)),]
group_list=factor(gl$MSI.Status)

counts=data.frame(t(counts))
counts <- cbind(
  counts, 
  MSI.Status = group_list
)

#Wilcox

wilcoxon_de <- c() # Initialize empty vector for p-values
genera <- colnames(counts)[1:(ncol(counts) - 1)]  
# Do "for loop" over selected column names
for (i in genera) {
  
  result <- wilcox.test(counts[, i] ~ MSI.Status,
                        data =counts)
  
  # Stores p-value to the vector with this column name
  wilcoxon_de[[i]]  <- result$p.value
  
}

wilcoxon_de <- data.frame(genes =  names(wilcoxon_de),
                         p_raw = unlist(wilcoxon_de))
wilcoxon_de$p_adjusted <- p.adjust(wilcoxon_de$p_raw, method = "fdr")
wilcoxon_de_result=wilcoxon_de %>% filter(p_raw<0.05)

write.csv(wilcoxon_de,'MSI_deg.csv')

#logFC
exp=eset1[,colnames(eset1) %in% rownames(gl)]
MSI=exp[, colnames(exp) %in% rownames(gl[gl$MSI.Status=='MSI',]) ]
MSS=exp[, colnames(exp) %in% rownames(gl[gl$MSI.Status=='MSS',]) ]
foldChanges=log2(rowMeans(MSS)/rowMeans(MSI))
foldChanges=data.frame(genes=names(foldChanges),
                       log2FC=log2(rowMeans(MSS)/rowMeans(MSI)))
mside=left_join(wilcoxon_de_result,foldChanges,by='genes')
mside_result=mside %>% filter(abs(log2FC)>=2)


exp_de=exp[rownames(exp) %in% mside_result$genes,]
write.csv(exp_de,'MSIde_exp.csv') # Now we get differentially expressed human genes


library(clusterProfiler)
degs.list=mside_result$genes
library(org.Hs.eg.db)
library(GOSemSim)
library(topGO)
library(Rgraphviz)
library(pathview)
erich.go.BP = enrichGO(gene =degs.list,
                       OrgDb = org.Hs.eg.db,
                       keyType = "SYMBOL",
                       ont = "BP",
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.2)
dotplot(erich.go.BP)

erich.go.CC = enrichGO(gene =degs.list,
                       OrgDb = org.Hs.eg.db,
                       keyType = "SYMBOL",
                       ont = "MF",
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.2)
dotplot(erich.go.CC)
R.utils::setOption( "clusterProfiler.download.method",'wininet')
hsa_kegg<-download_KEGG("hsa")
gene__entrez <- as.character(na.omit(bitr(degs.list, 
                                        fromType="SYMBOL", 
                                        toType="ENTREZID", 
                                        OrgDb="org.Hs.eg.db")[,2])) #"org.Hs.eg.db" "org.Mm.eg.db"


kk = enrichKEGG(gene =gene__entrez,
                       organism = 'hsa',
                       keyType = 'kegg',
                       pvalueCutoff = 0.05,
                       qvalueCutoff = 0.2)



search_kegg_organism('hsa', by='kegg_code')
