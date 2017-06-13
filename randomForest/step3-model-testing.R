rm(list=ls())
load('rf_output.Rdata')
load('ML_RF_input.Rdata')

library(randomForest)
library(ROCR)
require(Hmisc)

predictor_data=t(testing_data)
predictor_data[1:4,1:4]
dim(predictor_data)
RF_predictor_names=rownames(rf_output$importance)
predictor_data=predictor_data[,RF_predictor_names]
predictor_data[1:4,1:4]
dim(predictor_data)

RF_predictions_responses=predict(rf_output, predictor_data, type="response")
RF_predictions_votes=predict(rf_output, predictor_data, type="vote")
head(RF_predictions_responses)
head(RF_predictions_votes)

# RFS: In cancer, the length of time after primary treatment for a cancer ends 
# that the patient survives without any signs or symptoms of that cancer.
# In a clinical trial, measuring the RFS is one way to see how well a new treatment works. 
# Also called DFS, disease-free survival, and relapse-free survival.

head(testing_clinical)
clindata=testing_clinical[,c('event.rfs','time.rfs')]
clindata_plusRF=cbind(clindata,RF_predictions_responses,RF_predictions_votes)
dim(clindata_plusRF)
clindata_plusRF=clindata_plusRF[! is.na(clindata_plusRF$event.rfs)  ,]
dim(clindata_plusRF)
head(clindata_plusRF)

save(clindata_plusRF,file='predictor_output.Rdata')

confusion=table(clindata_plusRF[,c("event.rfs","RF_predictions_responses")])
rownames(confusion)=c("NoRelapse","Relapse")
confusion

sensitivity=(confusion[2,2]/(confusion[2,2]+confusion[2,1]))*100
sensitivity
specificity=(confusion[1,1]/(confusion[1,1]+confusion[1,2]))*100
specificity
overall_error=((confusion[1,2]+confusion[2,1])/sum(confusion))*100
overall_error
overall_accuracy=((confusion[1,1]+confusion[2,2])/sum(confusion))*100
overall_accuracy
class1_error=confusion[1,2]/(confusion[1,1]+confusion[1,2])
class1_error
class2_error=confusion[2,1]/(confusion[2,2]+confusion[2,1])
class2_error
 
# Create variables for the known target class and predicted class probabilities.
target=clindata_plusRF[,"event.rfs"]
target[target==1]="Relapse"
target[target==0]="NoRelapse"
relapse_scores=clindata_plusRF[,"Relapse"]
# First calculate the AUC value.
pred=prediction(relapse_scores,target)
perf_AUC=performance(pred,"auc")
AUC=perf_AUC@y.values[[1]]
AUC_out=paste("AUC=",AUC,sep="")
# Then, plot the actual ROC curve.
perf_ROC=performance(pred,"tpr","fpr")
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))





