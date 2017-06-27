###############
#             #
# Exercise 1  #
#             #
###############
rm(list=ls())
library(quantreg)
# Load the quantreg package and the barro dataset (Barro and Lee, 1994). 
# This has data on GDP growth rates for various countries
data(barro)
summary(barro)
str(barro)

###############
#             #
# Exercise 2  #
#             #
###############

# The dependent variable is y.net (Annual change per capita GDP). 

y <- barro$y.net

x<- cbind(barro$lgdp2,
          barro$mse2,
          barro$fse2,
          barro$fhe2,
          barro$mhe2,
          barro$lexp2,
          barro$lintr2,
          barro$gedy2,
          barro$Iy2,
          barro$gcony2,
          barro$lblakp2,
          barro$pol2,
          barro$ttrad2)

colnames(x) <- c("Initial Per Capita GDP",
                 "Male Secondary Education",
                 "Female Secondary Education",
                 "Female Higher Education",
                 "Male Higher Education",
                 "Life Expectancy",
                 "Human Capital",
                 "Education/GDP",
                 "Investment/GDP",
                 "Public Consumption/GDP",
                 "Black Market Premium",
                 "Political Instability",
                 "Growth Rate Terms Trade")

###############
#             #
# Exercise 3  #
#             #
###############
str(x);class(x)  ## matrix 

# Regress y.net on the independent variables using OLS. 
# We will use this result as benchmark for comparison.

model3 <- lm(y ~ x)
summary(model3)

###############
#             #
# Exercise 4  #
#             #
###############

# Using the rq function, estimate the model at the median y.net. 
model4 <- rq(y ~ x, tau = 0.5)
summary(model4, se="rank")

###############
#             #
# Exercise 5  #
#             #
###############

# Estimate the model for the first and third quartiles and compare results.
model5a <- rq(y ~ x, tau = 0.25)
summary(model5a, se="rank")


model5b <- rq(y ~ x, tau = 0.75)
summary(model5b, se="rank")


###############
#             #
# Exercise 6  #
#             #
###############

# Using a single command estimate the model for 10 equally spaced deciles of y.net.
model6 <- rq(y ~ x, tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
summary(model6, se="rank")



###############
#             #
# Exercise 7  #
#             #
###############

# quantreg package also offers shrinkage estimators to determine which variables play the most important role in predicting y.net.

model7 <- rq.fit.lasso(x, y, tau = 0.5, lambda = 0.5)
print(model7$coefficients)

# Note that some beta estimates have been shrunk closer to zero.

###############
#             #
# Exercise 8  #
#             #
###############

# Quantile plots are most useful for interpreting results. 
tau_seq <- seq(0.05, 0.95, by=0.05)

###############
#             #
# Exercise 9  #
#             #
###############

# Use the result from exercise-8 to plot the graphs. 
model8 <- rq(y ~ x, tau = tau_seq)
plot(summary(model8))
# Note that the red line is the OLS estimate bounded by the dotted lines which represent confidence intervals.

# Note that 'gcony2' is different from the OLS estimate at 
# lower quantiles of 'y.net'.

################
#              #
# Exercise 10  #
#              #
################

anova(model5a, model5b)

#  test whether coefficients are significantly different for the first and third quartile based regressions.




