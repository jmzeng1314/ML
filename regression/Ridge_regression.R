###############
#             #
# Exercise 1  #
#             #
###############

library(lars)
library(glmnet)

data(diabetes)
attach(diabetes)
# 
# The dataset has three matrices x, x2 and y. 
# x has a smaller set of independent variables while x2 contains the full set with quadratic and interaction terms. 
# y is the dependent variable which is a quantitative measure of the progression of diabetes.

set.seed(1234)

par(mfrow=c(2,5))
for(i in 1:10){
  plot(x[,i], y)
  abline(lm(y~x[,i]))
}


layout(1)

model_ols <- lm(y ~ x)
summary(model_ols)

###############
#             #
# Exercise 2  #
#             #
###############
# Fit the ridge regression model using the glmnet function 

lambdas <- 10^seq(7, -3)
model_ridge <- glmnet(x, y, alpha = 0, lambda = lambdas)
plot.glmnet(model_ridge, xvar = "lambda", label = TRUE)


###############
#             #
# Exercise 3  #
#             #
###############

# Use the cv.glmnet function to get the cross validation curve 
# and the value of lambda that minimizes the mean cross validation error.

# 
cv_fit <- cv.glmnet(x=x, y=y, alpha = 0, nlambda = 1000)
plot.cv.glmnet(cv_fit)


cv_fit$lambda.min

###############
#             #
# Exercise 4  #
#             #
###############

# Using the minimum value of lambda from the previous exercise, get the estimated beta matrix

fit <- glmnet(x=x, y=y, alpha = 0, lambda=cv_fit$lambda.min)
fit$beta
# Note that coefficients are lower than least squares estimates.

###############
#             #
# Exercise 5  #
#             #
###############

# To get a more parsimonious model we can use a higher value of lambda that is within one standard error of the minimum.

fit <- glmnet(x=x, y=y, alpha = 0, lambda=cv_fit$lambda.1se)
fit$beta
# Note the shrinkage effect on the estimates.
###############
#             #
# Exercise 6  #
#             #
###############

# Split the data randomly between a training set (80%) and test set (20%).

library(caret)

intrain <- createDataPartition(y=diabetes$y,
                               p = 0.8,
                               list = FALSE)
training <- diabetes[intrain,]

testing <- diabetes[-intrain,]


###############
#             #
# Exercise 7  #
#             #
###############

cv_ridge <- cv.glmnet(x=training$x, y=training$y,
                      alpha = 0, nlambda = 1000)
ridge_reg <- glmnet(x=training$x, y=training$y,
                    alpha = 0, lambda=cv_ridge$lambda.min)
ridge_reg$beta


ridge_reg <- glmnet(x=training$x, y=training$y,
                    alpha = 0, lambda=cv_ridge$lambda.1se)
ridge_reg$beta

###############
#             #
# Exercise 8  #
#             #
###############

# Get predictions from the ridge regression model for the test set and calculate the prediction standard error. 

ridge_reg <- glmnet(x=training$x, y=training$y,
                    alpha = 0, lambda=cv_ridge$lambda.min)
ridge_pred <- predict.glmnet(ridge_reg,
                             s = cv_ridge$lambda.min, newx = testing$x)
sd((ridge_pred - testing$y)^2)/sqrt(length(testing$y)) # [1] 517.6902


ridge_reg <- glmnet(x=training$x, y=training$y,
                    alpha = 0, lambda=cv_ridge$lambda.1se)
ridge_pred <- predict.glmnet(ridge_reg,
                             s = cv_ridge$lambda.1se, newx = testing$x)
sd((ridge_pred - testing$y)^2)/sqrt(length(testing$y))   # [1] 497.2364


# lower prediction error with higher lambda

###############
#             #
# Exercise 9  #
#             #
###############
# Fit the least squares model on the training set.
ols_reg <- lm(y ~ x, data = training)
summary(ols_reg)
################
#              #
# Exercise 10  #
#              #
################
# Get predictions from the least squares model for the test set and calculate the prediction standard error.
ols_pred <- predict(ols_reg, newdata=testing$x, type = "response")
sd((ols_pred - testing$y)^2)/sqrt(length(testing$y)) # [1] 528.9672
 
# least squares prediction error is higher.





