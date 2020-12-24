library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# The regsubsets() function (part of the leaps library) performs best subset 
# selection by identifying the best model that contains a given number of 
# predictors, where best is quantified using RSS
library(leaps)
regfit.full = regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

# By default, regsubsets() only reports results up to the best 
# eight-variable model. But the nvmax option can be used in order 
# to return as many variables as are desired. Here we fit up to a 
# 19-variable model
regfit.full = regsubsets(Salary ~ . , data = Hitters, nvmax = 19) 
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
# For instance, we see that the R2 statistic increases from 32 %,
# when only one variable is included in the model, to almost 55%, when all 
# variables are included. As expected, the R2 statistic increases monotonically 
# as more variables are included.

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will 
# help us decide which model to select. 
par(mar = rep(2, 4))
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables ", ylab = "RSS",
       type = "l")
plot(reg.summary$adjr2, xlab="Number of Variables ",
       ylab = "Adjusted RSq", type = "l")

which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# In a similar fashion we can plot the Cp and BIC statistics, and 
# indicate the models with the smallest statistic using which.min().
plot(reg.summary$cp, xlab = "Number of Variables ", ylab="Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp [10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables ", ylab = "BIC",
       type = "l")
points(6, reg.summary$bic [6], col = "red", cex = 2, pch = 20)

# Alternative Plot
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full ,6)
