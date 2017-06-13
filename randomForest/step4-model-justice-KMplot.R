rm(list=ls())
load('predictor_output.Rdata') 

library(survival)

quantiles=quantile(clindata_plusRF[,"Relapse"], probs=c(0.33333,0.66667)) 

clindata_plusRF$RF_Group2=ifelse(clindata_plusRF$Relapse > quantiles[2],'high',
                                 ifelse(clindata_plusRF$Relapse > quantiles[1],"int","low")) 
head(clindata_plusRF)

clindata_plusRF[,"e_rfs_10yrcens"]=clindata_plusRF[,"event.rfs"]
clindata_plusRF[which(clindata_plusRF[,"time.rfs"]>10),"e_rfs_10yrcens"]=0
surv_data=clindata_plusRF[,c("time.rfs","e_rfs_10yrcens","RF_Group2")]

head(surv_data)
surv_data.surv = with(surv_data, Surv(time.rfs, e_rfs_10yrcens==1))
#Calculate p-value
survdifftest=survdiff(surv_data.surv ~ RF_Group2, data = surv_data)
survpvalue = 1 - pchisq(survdifftest$chisq, length(survdifftest$n) - 1)
survpvalue = format(as.numeric(survpvalue), digits=3)

surv_data_lin=clindata_plusRF[,c("time.rfs","e_rfs_10yrcens","RF_Group2")]
surv_data_lin[,"RF_Group2"]=as.vector(surv_data_lin[,"RF_Group2"])
surv_data_lin[which(surv_data_lin[,"RF_Group2"]=="low"),"RF_Group2"]=1
surv_data_lin[which(surv_data_lin[,"RF_Group2"]=="int"),"RF_Group2"]=2
surv_data_lin[which(surv_data_lin[,"RF_Group2"]=="high"),"RF_Group2"]=3
surv_data_lin[,"RF_Group2"]=as.numeric(surv_data_lin[,"RF_Group2"])
survpvalue_linear=summary(coxph(Surv(time.rfs, e_rfs_10yrcens)~RF_Group2, data=surv_data_lin))$sctest[3]
survpvalue_linear = format(as.numeric(survpvalue_linear), digits=3)

krfit.by_RFgroup = survfit(surv_data.surv ~ RF_Group2, data = surv_data) 
colors = rainbow(5)
title="Survival by RFRS - Test Set"
plot(krfit.by_RFgroup, col = colors, xlab = "Time (Years)", ylab = "Relapse Free Survival", main=title, cex.axis=1.3, cex.lab=1.4)
abline(v = 10, col = "black", lty = 3)
groups=sort(unique(surv_data[,"RF_Group2"])) #returns unique factor levels sorted alphabetically
names(colors)=groups
groups_custom=c("low","int","high")
colors_custom=colors[groups_custom]
group_sizes_custom=table(surv_data[,"RF_Group2"])[groups_custom]
groups_custom=c("Low","Intermediate","High") #Reset names
legend_text=c(paste(groups_custom, " ", "(", group_sizes_custom, ")", sep=""),paste("p =", survpvalue_linear, sep=" "))
legend(x = "bottomleft", legend = legend_text, col = c(colors_custom,"white"), lty = "solid", bty="n", cex=1.2)
