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

cv_fit$lambda.1se
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta




model_ols2 <- lm(y~x2)
summary(model_ols2)

model_lasso1 <- glmnet(x2, y)
plot.glmnet(model_lasso1, xvar = "norm", label = TRUE)
cv_fit1 <- cv.glmnet(x=x2, y=y, alpha = 1, nlambda = 1000)
plot.cv.glmnet(cv_fit1)
fit1 <- glmnet(x=x2, y=y, alpha = 1, lambda=cv_fit1$lambda.min)
fit1$beta













