rm(list=ls())
library(lars)
# https://cran.r-project.org/web/packages/lars/lars.pdf
library(glmnet)
data(diabetes)
attach(diabetes)
summary(x)

par(mfrow=c(2,5))
for(i in 1:10){
  plot(x[,i], y)
  abline(lm(y~x[,i]))
}

model_ols <- lm(y ~ x)
summary(model_ols)

model_lasso <- glmnet(x, y)
plot.glmnet(model_lasso, xvar = "norm", label = TRUE)

cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000)
plot.cv.glmnet(cv_fit) 

cv_fit$lambda.min
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta
coef(cv_fit$glmnet.fit, s =  min(cv_fit$lambda))


cv_fit$lambda.1se
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta
coef(cv_fit$glmnet.fit, s = cv_fit$lambda.1se)



