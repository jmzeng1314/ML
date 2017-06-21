rm(list = ls())
setwd('lasso/')
GBM_miRNA=read.table('GBM_miRNA_array.txt',stringsAsFactors = F,sep=  '\t',header = T)
GBM_miRNA=GBM_miRNA[-1,]
rownames(GBM_miRNA)=GBM_miRNA[,1]
GBM_miRNA=GBM_miRNA[,-1]

#Now I need to identify normal and tumor samples. this is done using the TCGA barcode (https://wiki.nci.nih.gov/display/TCGA/TCGA+barcode). The two digits at position 14-15 of the barcode will indicate teh sample type, from the link:
#  "Tumor types range from 01 - 09, normal types from 10 - 19 and control samples from 20 - 29."
samples=colnames(GBM_miRNA)
table(substr(samples,14,14))

GBM_miRNA=GBM_miRNA[,substr(samples,14,14)==0]      
samples=colnames(GBM_miRNA)
table(substr(samples,14,14))

x=apply(GBM_miRNA, 1, as.numeric)
samples=tolower(substr(samples,1,12))



GBM_clinical=read.table('GBM.clin.merged.picked.txt',stringsAsFactors = F,sep=  '\t',header = T)
rownames(GBM_clinical)=GBM_clinical[,1]
GBM_clinical=GBM_clinical[,-1]
GBM_clinical=GBM_clinical[-1,]
head(GBM_clinical[,1:6])
table(is.na(GBM_clinical['days_to_death',]))
table(is.na(GBM_clinical['days_to_last_followup',]))
table( as.numeric(GBM_clinical['vital_status',])) 



library(lars)
# https://cran.r-project.org/web/packages/lars/lars.pdf
library(glmnet)

model_ols <- lm(y ~ x)
summary(model_ols)




