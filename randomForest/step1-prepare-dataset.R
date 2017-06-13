library(GEOquery)
library(limma) 

GSE2034 <- getGEO('GSE2034', destdir=".",getGPL = F)
GSE2034[[1]] 
GSE2990 <- getGEO('GSE2990', destdir=".",getGPL = F)
GSE2990[[1]]
save(GSE2034,GSE2990,file = 'ML_RF_GEO.Rdata')

load('ML_RF_GEO.Rdata')

if(T){
  library("annotate")
  platformDB='hgu133a.db'
  library(platformDB, character.only=TRUE) 
  probe2symbol=toTable(hgu133aSYMBOL)
  
  rmDupID <-function(a=matrix(c(1,1:5,2,2:6,2,3:7),ncol=6)){
    exprSet=a[,-1]
    rowMeans=apply(exprSet,1,function(x) mean(as.numeric(x),na.rm=T))
    a=a[order(rowMeans,decreasing=T),]
    exprSet=a[!duplicated(a[,1]),]
    #
    exprSet=exprSet[!is.na(exprSet[,1]),]
    rownames(exprSet)=exprSet[,1]
    exprSet=exprSet[,-1]
    return(exprSet)
  } 
  

}

exprSet=exprs(GSE2034[[1]])
keep_probe=rownames(exprSet) %in% probe2symbol$probe_id
exprSet=exprSet[keep_probe,]
exprSet <- cbind( probe2symbol[match(rownames(exprSet),probe2symbol$probe_id),2],exprSet)
exprSet <- rmDupID(exprSet)
rn=rownames(exprSet)
exprSet=apply(exprSet,2,as.numeric)
rownames(exprSet)=rn
exprSet[1:4,1:4]
exprSet=log2(exprSet) ## based on 2
#boxplot(exprSet,las=2)
GSE2034_exprSet=exprSet
GSE2034_metadata=pData(GSE2034[[1]])
 
training_data=GSE2034_exprSet
training_clinical=GSE2034_metadata[,c("title","geo_accession","characteristics_ch1")]
head(training_clinical)


exprSet=exprs(GSE2990[[1]])
keep_probe=rownames(exprSet) %in% probe2symbol$probe_id
exprSet=exprSet[keep_probe,]
exprSet <- cbind( probe2symbol[match(rownames(exprSet),probe2symbol$probe_id),2],exprSet)
exprSet <- rmDupID(exprSet)
rn=rownames(exprSet)
exprSet=apply(exprSet,2,as.numeric)
rownames(exprSet)=rn
exprSet[1:4,1:4]
#exprSet=log2(exprSet) ## based on 2
#boxplot(exprSet,las=2)
GSE2990_exprSet=exprSet
GSE2990_metadata=pData(GSE2990[[1]])


testing_data=GSE2990_exprSet
# testing_clinical=GSE2990_metadata[,c("title","geo_accession","characteristics_ch1.1", "characteristics_ch1.2",
#                                     # "characteristics_ch1.2","characteristics_ch1.3","characteristics_ch1.4", 
#                                      "characteristics_ch1.6","characteristics_ch1.7","characteristics_ch1.8" 
#                                      )]


testing_clinical=read.table('ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE2nnn/GSE2990/suppl/GSE2990_suppl_info.txt',
                            header = T,stringsAsFactors = F,sep = '\t')
tail(testing_clinical)


save(training_data,training_clinical,testing_data,testing_clinical,file = 'ML_RF_input.Rdata')
load('ML_RF_input.Rdata')

if(F){
  dir_cels='GSE2034_RAW'
  data <- ReadAffy(celfile.path=dir_cels) 
  eset <- rma(data)
  calls <- mas5calls(data) # get PMA calls
  calls <- exprs(calls)
  absent <- rowSums(calls == 'A') # how may samples are each gene 'absent' in all samples
  absent <- which (absent == ncol(calls)) # which genes are 'absent' in all samples
  rmaFiltered <- eset[-absent,] # filters out the genes 'absent' in all samples
  expr_matrix=exprs(rmaFiltered)
  
}



