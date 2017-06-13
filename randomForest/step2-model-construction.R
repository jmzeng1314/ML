rm(list = ls())
load('ML_RF_input.Rdata')

library(randomForest)
library(ROCR)
library(genefilter)
library(Hmisc)

dim(training_data)
training_data[1:4,1:4]
 
ffun=filterfun(pOverA(p = 0.2, A = 100), cv(a = 0.7, b = 10))
filt=genefilter(2^training_data,ffun)
filt_Data=training_data[filt,]

dim(filt_Data)
filt_Data[1:4,1:4]
predictor_data=t(filt_Data)
str(predictor_data)


head(training_clinical)
target=as.numeric(sapply(as.character(training_clinical$characteristics_ch1),function(x) strsplit(x,':')[[1]][2]))
target[target==0]="NoRelapse"
target[target==1]="Relapse"
target=as.factor(target)
head(target)


tmp = as.vector(table(target))
num_classes = length(tmp)
min_size = tmp[order(tmp,decreasing=FALSE)[1]]
sampsizes = rep(min_size,num_classes)
rf_output=randomForest(x=predictor_data, y=target, importance = TRUE, ntree = 10001, proximity=TRUE, sampsize=sampsizes)

save(rf_output,file='rf_output.Rdata')

rf_importances=importance(rf_output, scale=FALSE)
head(rf_importances)
confusion=rf_output$confusion
confusion
sensitivity=(confusion[2,2]/(confusion[2,2]+confusion[2,1]))*100
sensitivity
specificity=(confusion[1,1]/(confusion[1,1]+confusion[1,2]))*100
specificity
overall_error=rf_output$err.rate[length(rf_output$err.rate[,1]),1]*100
overall_error
overall_accuracy=1-overall_error
overall_accuracy
class1_error=paste(rownames(confusion)[1]," error rate= ",confusion[1,3], sep="")
class1_error
class2_error=paste(rownames(confusion)[2]," error rate= ",confusion[2,3], sep="")
class2_error
overall_accuracy=100-overall_error
overall_accuracy










