 
############################################################ 
#####################     Exercise 1   ##################### 
#######  Load the MASS package and the biopsy dataset, 
#######  then prepare your data to be feed to a neural network.
############################################################

library(MASS)
data<-biopsy
dim(data)

scale.0.1<-function(x){
  (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
}

norm.data<-data.frame(lapply(data[,2:10], function(x) scale.0.1(x)))
norm.data$class<-data$class
str(norm.data)
sum(is.na(norm.data))
## rm NA 
norm.data<-norm.data[complete.cases(norm.data),]

set.seed(42)
index<-sample(1:nrow(norm.data),round(0.75*nrow(norm.data)),replace=FALSE)
train<-norm.data[index,]
test<-norm.data[-index,]
dim(train)
dim(test)

############################################################ 
#####################     Exercise 2   ##################### 
#######  do a logistic regression on the biopsy data set 
#######  using a feedforward neural network.
############################################################

library(nnet)
cross.val.nnet<-function(train,test,low_range,high_range){
  
  acc<-NULL
  for (h in low_range:high_range)
  {
    temp.nn<-nnet(class~.,size=h,data=train)
    pred<-predict(temp.nn,test,type="class")
    
    Table<-table(test$class,pred)
    accuracy<-sum(diag(Table))/sum(Table)
    acc<-c(acc,accuracy)
  }
  
  return(acc)
}

####################
#                  #
#    Exercise 3    #
#                  #
####################
# Use your function on your data set and plot the result. 
set.seed(42)
cross.val.nnet(train,test,1,9)
####################
#                  #
#    Exercise 4    #
#                  #
####################
library(neuralnet)
## change 'benigh' and 'malignant' to 1 and 2 
train$class<-as.numeric(train$class)
test$class<-as.numeric(test$class)

n <- names(train)
f <- as.formula(paste("class ~", paste(n[!n %in% "class"], collapse = " + ")))
f
## Use the neuralnet() function with the default parameter
set.seed(42)
model.neuralnet.1<-neuralnet(f,data=train,hidden=6)

pred.model.1 <- compute(model.neuralnet.1,test[,1:9])
str(pred.model.1)

pred.model.1$net.result <-round(pred.model.1$net.result)
Table<-table(test$class,pred.model.1$net.result)
Table 
accuracy<-sum(diag(Table))/sum(Table)
accuracy

####################
#                  #
#    Exercise 5    #
#                  #
####################
# Use the neuralnet() function with the parameter algorithm set to ‘rprop-‘, 
# which stand for resilient backpropagation without weight backtracking.
set.seed(42)
model.neuralnet.2<-neuralnet(f, data=train, hidden=6, algorithm ='rprop-')

pred.model.2 <- compute(model.neuralnet.2,test[,1:9])
pred.model.2$net.result<-round(pred.model.2$net.result)
pred.model.2$net.result[which(pred.model.2$net.result<=1)]<-1
pred.model.2$net.result[which(pred.model.2$net.result>=2)]<-2

Table<-table(test$class,pred.model.2$net.result)
Table
accuracy<-sum(diag(Table))/sum(Table)
accuracy

####################
#                  #
#    Exercise 6    #
#                  #
#################### 
set.seed(42)
# use the globally convergent algorithm (grprop) 
# and to modify the learning rate associated with the smallest absolute gradient (sag) 
model.neuralnet.3<-neuralnet(f, data=train, hidden=6, 
                             algorithm ='sag',learningrate.limit=c(0.01,1),
                             stepmax = 1e+06)
 
pred.model.3 <- compute(model.neuralnet.3,test[,1:9]) 
pred.model.3$net.result <-round(pred.model.3$net.result)

Table<-table(test$class,pred.model.3$net.result)
Table 
accuracy<-sum(diag(Table))/sum(Table)
accuracy

# use the globally convergent algorithm (grprop) 
# and to modify the learning rate associated with the smallest learning rate (slr)

set.seed(42)
model.neuralnet.4<-neuralnet(f, data=train, hidden=6, 
                             algorithm ='slr',
                             learningrate.limit=c(0.01,1), stepmax = 1e+06)
 
pred.model.4 <- compute(model.neuralnet.4,test[,1:9]) 
pred.model.4$net.result <-round(pred.model.4$net.result)

Table<-table(test$class,pred.model.4$net.result)
Table


####################
#                  #
#    Exercise 7    #
#                  #
####################
# The learning rate determine how much the backpropagation 
# can affect the weight at each iteration. 
#  high learning rate  ~~~ learn a lot of each observation ~~outlier could easily affect your weight 
# A small learning rate~~ learn less from each observation ~~your neural network is less affected by outlier

set.seed(42)
model.neuralnet.5<-neuralnet(f, data=train, hidden=6, algorithm ='rprop-', 
                             learningrate=0.001)

pred.model.5 <- compute(model.neuralnet.5,test[,1:9])
pred.model.5$net.result <-round(pred.model.5$net.result)

Table<-table(test$class,pred.model.5$net.result)
Table
pred.model.5$net.result[which(pred.model.5$net.result<=1)]<-1
pred.model.5$net.result[which(pred.model.5$net.result>=2)]<-2

Table<-table(test$class,pred.model.5$net.result)
Table
accuracy<-sum(diag(Table))/sum(Table)
accuracy
set.seed(42)
model.neuralnet.6<-neuralnet(f, data=train, hidden=6, algorithm ='rprop-', 
                             learningrate=1)

pred.model.6 <- compute(model.neuralnet.6,test[,1:9])
pred.model.6$net.result <-round(pred.model.6$net.result)

Table<-table(test$class,pred.model.6$net.result)
Table

pred.model.6$net.result[which(pred.model.6$net.result==0)]<-1

Table<-table(test$class,pred.model.6$net.result)
Table
accuracy<-sum(diag(Table))/sum(Table)
accuracy

####################
#                  #
#    Exercise 8    #
#                  #
####################
plot(model.neuralnet.5)
##  make a visual representation of the neural network you made


####################
#                  #
#    Exercise 9    #
#                  #
####################
## feedfordward neural network with one hidden layer of neurons
## Create a feedforward neural network with three hidden layers of nine neurons 

set.seed(42) 
model.neuralnet.7<-neuralnet(f, data=train, hidden=c(9,9,9)) 
pred.model.7 <- compute(model.neuralnet.7,test[,1:9]) 
pred.model.7$net.result <-round(pred.model.7$net.result) 
Table<-table(test$class,pred.model.7$net.result) 
Table

accuracy<-sum(diag(Table))/sum(Table)
accuracy

####################
#                  #
#    Exercise 10   #
#                  #
####################
plot(model.neuralnet.7)


