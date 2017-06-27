rm(list=ls())
####################
#                  #
#    Exercise 1    #
#                  #
####################
#a. 
data(state)

#b. 
state77 <- as.data.frame(state.x77)
str(state77)
#c.
names(state77)[4] <- "Life.Exp"
names(state77)[6] <- "HS.Grad"


####################
#                  #
#    Exercise 2    #
#                  #
####################

# Suppose we wanted to enter all the variables in a first-order linear regression model 
# with Life Expectancy as the dependent variable.
model <- lm(Life.Exp ~ ., data=state77) #the '.' means 'all'
summary(model)

####################
#                  #
#    Exercise 3    #
#                  #
####################

# Suppose we wanted to remove the Income, Illiteracy, and Area variables from the model 
model2 <- update(model, . ~ . -Income -Illiteracy -Area) #the '.' means 'same as in original model'
summary(model2)

####################
#                  #
#    Exercise 4    #
#                  #
####################

# Let's assume that we have settled on a model that has HS.Grad and Murder as predictors.
model3 <- lm(Life.Exp ~ HS.Grad + Murder, data=state77)
summary(model3)



####################
#                  #
#    Exercise 5    #
#                  #
####################
# Add an interaction term to the model 
model4 <- lm(Life.Exp ~ HS.Grad + Murder + HS.Grad:Murder, data=state77)
summary(model4)

model4 <- lm(Life.Exp ~ (HS.Grad+Murder)^2, data=state77)
summary(model4)

####################
#                  #
#    Exercise 6    #
#                  #
####################

# Obtain 95% confidence intervals for the coefficients of the two predictor variables.
confint(model3, level=0.95)

####################
#                  #
#    Exercise 7    #
#                  #
####################

# Predict the Life Expectancy for a state where 55% of the population are High School graduates, 
# and the murder rate is 8 per 100,000.

predict(model3,data.frame(HS.Grad=55,Murder=8))

####################
#                  #
#    Exercise 8    #
#                  #
####################
predict(model3,data.frame(HS.Grad=55,Murder=8),interval="confidence",level=0.98)

####################
#                  #
#    Exercise 9    #
#                  #
####################
predict(model3,data.frame(HS.Grad=55,Murder=8),interval="prediction",level=0.98)

####################
#                  #
#    Exercise 10   #
#                  #
####################

# Since our model only has two predictor variables, we can generate a 3D plot of our data and the fitted regression plane. 

library(rgl)
plotdat <- expand.grid(HS.Grad=seq(34,70,by=2),Murder=seq(1,16,by=1))
plotdat$pred1 <- predict(model3,newdata=plotdat)
with(state77,plot3d(HS.Grad,Murder,Life.Exp,  col="blue", size=1, type="s"))
with(plotdat,surface3d(unique(HS.Grad),unique(Murder),pred1,
                       alpha=0.5,front="line", back="line"))




####################
#                  #
#    Exercise 1    #
#                  #
####################
#a. 
data(state)

#b. 
state77 <- as.data.frame(state.x77)

#c.
names(state77)[4] <- "Life.Exp"
names(state77)[6] <- "HS.Grad"

#d.
round(cor(state77),3) #displays correlations to 3 decimal places
#e.
pairs(~ Life.Exp + HS.Grad + Murder + Frost, data=state77, gap=0)

####################
#                  #
#    Exercise 2    #
#                  #
####################
#a.
model <- lm(Life.Exp ~ HS.Grad + Murder, data=state77)
summary(model)

#b.
resids <- model$residuals

#c. 
fitted <- model$fitted.values

####################
#                  #
#    Exercise 3    #
#                  #
####################
#a.
plot(fitted,resids,main="Residual Plot",xlab="Fitted Values",ylab="Residuals")
abline(h=0,col="red")


#b.
plot(model,which=1)

####################
#                  #
#    Exercise 4    #
#                  #
####################
par(mfrow=c(1,2)) # draw the two plots side by side

plot(state77$HS.Grad,resids,main="Residuals vs. HS.Grad",xlab="HS.Grad",ylab="Residuals")
abline(h=0,col="red")
plot(state77$Murder,resids,main="Residuals vs. Murder",xlab="Murder",ylab="Residuals")
abline(h=0,col="red")

par(mfrow=c(1,1)) # restore to the default

####################
#                  #
#    Exercise 5    #
#                  #
####################
#a.
qqnorm(resids,ylab="Residuals")
qqline(resids)

#b.
plot(model,which=2)

####################
#                  #
#    Exercise 6    #
#                  #
####################
#a.  
stzed <- rstudent(model)

#b.  
stzed[abs(stzed) > 2]

####################
#                  #
#    Exercise 7    #
#                  #
####################
#a.
lever <- hat(model.matrix(model))
plot(lever)

#b.
#obtain the threshold
thresh2 <- 2*length(model$coefficients)/length(lever)

#print leverage values above threshold
lever[lever > thresh2]

rownames(state77)[which(lever > thresh2)]
## [1] "Alaska" "Nevada"

####################
#                  #
#    Exercise 8    #
#                  #
####################
#a.
dffits1 <- dffits(model)

#b.
thresh3 <- 2*sqrt(length(model$coefficients)/length(dffits1))
dffits1[dffits1 > thresh3]

#c.
dfbetas1 <- dfbetas(model)

#d.
thresh4 <- 2/sqrt(length(dfbetas1[,1]))
dfbetas1[dfbetas1[,1] > thresh4,1]  #for intercept

dfbetas1[dfbetas1[,2] > thresh4,2]  #for HS.Grad

dfbetas1[dfbetas1[,3] > thresh4,3]  #for Murder

####################
#                  #
#    Exercise 9    #
#                  #
####################
#a.
cooksd <- cooks.distance(model)
plot(cooksd,ylab="Cook's Distance")
plot of chunk mlr-exercises2
#b. 
plot(model,which=4)
plot of chunk mlr-exercises2
#c.
thresh <- 4/length(resids)
cooksd[cooksd > thresh] 

####################
#                  #
#    Exercise 10   #
#                  #
####################
library(car)
influencePlot(model, main="Influence Plot")



data(state)
state77 <- as.data.frame(state.x77)
names(state77)[4] <- "Life.Exp"
names(state77)[6] <- "HS.Grad"


####################
#                  #
#    Exercise 1    #
#                  #
####################
#a.
library(car)
m1 <- lm(Life.Exp ~ HS.Grad+Murder, data=state77)
avPlots(m1)
#Note that the slope of the line is positive in the HS.Grad plot, and negative in the Murder plot, as expected. 

#b.
avPlots(m1,id.method=list("mahal"),id.n=2)

####################
#                  #
#    Exercise 2    #
#                  #
####################
#a.
with(state77,avPlot(lm(Life.Exp ~ HS.Grad+Murder+Illiteracy),variable=Illiteracy))
#Note that the slope is positive, contrary to what is expected

#b.
avPlots(lm(Life.Exp ~ .,data=state77), terms= ~ Population+Area)


####################
#                  #
#    Exercise 3    #
#                  #
####################
crPlots(lm(Life.Exp ~ HS.Grad+Murder+Income+Area,data=state77))

####################
#                  #
#    Exercise 4    #
#                  #
####################
ceresPlots(lm(Life.Exp ~ HS.Grad+Murder+Income+Area,data=state77))
####################
#                  #
#    Exercise 5    #
#                  #
####################
vif(lm(Life.Exp ~ .,data=state77))

####################
#                  #
#    Exercise 6    #
#                  #
####################
library(lmtest)
bptest(m1)

#There is no evidence of heteroscedasticity (of the type that depends on a linear combination of the predictors).

####################
#                  #
#    Exercise 7    #
#                  #
####################
ncvTest(m1)


####################
#                  #
#    Exercise 8    #
#                  #
####################
bptest(m1,varformula= ~ I(HS.Grad^2)+I(Murder^2)+HS.Grad*Murder,data=state77)

####################
#                  #
#    Exercise 9    #
#                  #
####################
#a. 
ks.test(m1$residuals,"pnorm")
####################
#                  #
#    Exercise 10   #
#                  #
####################
durbinWatsonTest(m1)











## 