#install.packages("BiocManager")  # BiocManager is required to install Bioconductor packages
#BiocManager::install(c("GEOquery", "limma"))
#BiocManager::install("biomaRt")
install.packages("dplyr")


rm(list=ls())
library(biomaRt)
library(dplyr)

setwd("C:/MSc Internship")
SG_GE <- read.csv("SingaporeCRC_data/SG-BULK_salmonTPM.csv")

mart <- useMart("ensembl","hsapiens_gene_ensembl")
dataset = listDatasets(mart)
mydataset = useDataset("hsapiens_gene_ensembl", mart=mart)
hg_symbols <- getBM(attribute=c('ensembl_gene_id','external_gene_name'),
                    filters = 'ensembl_gene_id', values = SG_GE$patient_id, mart = mydataset)

#This step is to clean the unrecognized parts
hg_symbols <- hg_symbols[which(hg_symbols$external_gene_name != ""),]

countf2 <- left_join(hg_symbols, SG_GE, by=c("ensembl_gene_id"="patient_id"))
countf2 <- aggregate(x = countf2[,3:ncol(countf2)],
                     by = list(symbol = countf2$external_gene_name),
                     FUN = max)
column_to_rownames(var = 'symbol')
head(countf2)

write.csv(hg_symbols, "SingaporeCRC_data/gene_names.csv")

#---------------------------- KMAP (see code in website; require their API to use)

install.packages("httr")
library(httr)

base_url <- "https://apimlqv2.tenwiseservice.nl/api/mlquery"
request <- GET(url = paste0(base_url, 'start/'))
payload <- list('apikey' = APIKEY,
                'csrfmiddlewaretoken' = cookies(request)$value)

symbols <- hg_symbols[,2]
symbols_df <- data.frame(external_gene_name = symbols)
payload[['terms']] <- paste0(symbols_df$external_gene_name)

request <- POST(header,
                url = paste0(base_url, "concept/search/"),
                body = payload
                )

converted <- content(request)
