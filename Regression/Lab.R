
# Libraries ---------------------------------------------------------------
library(MASS)
library(ISLR)
library(car)

# Simple Linear Regression ------------------------------------------------
names(Boston)
lm.fit = lm(medv~lstat, data = Boston)
summary(lm.fit)
confint(lm.fit)

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval ="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval ="prediction")

# The 95 % confidence interval associated with a lstat value of 
# 10 is (24.47, 25.63), and the 95 % prediction interval is (12.828, 37.28).
# As expected, the confidence and prediction intervals are centered around the 
# same point (a predicted value of 25.05 for medv when lstat equals 10), 
# but the latter are substantially wider.

## Plots
plot(lstat ,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
# residual plot
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# leverage points
plot(hatvalues (lm.fit))
which.max(hatvalues (lm.fit))

# Multiple Linear Regression ----------------------------------------------
lm.fit=lm(medv~lstat+age,data=Boston) 
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)
vif(lm.fit)

# removing age
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

# Interaction Terms -------------------------------------------------------

# The syntax lstat:black tells R to include an interaction term between lstat 
# and black. The syntax lstat*age simultaneously includes lstat, age, and the 
# interaction term lstat×age as predictors; it is a shorthand 
# for lstat+age+lstat:age.

summary(lm(medv~lstat*age,data=Boston))


# Non-linear Transformations of the Predictors ----------------------------

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# The near-zero p-value associated with the quadratic term suggests that 
# it leads to an improved model. We use the anova() function to further 
# quantify the extent to which the quadratic fit is superior to the linear fit.

lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)

# The anova() function performs a hypothesis test comparing the two models. 
# The null hypothesis is that the two models fit the data equally well, 
# and the alternative hypothesis is that the full model is superior. 
# Here the F-statistic is 135 and the associated p-value is virtually zero. 
# This provides very clear evidence that the model containing the 
# predictors lstat and lstat2 is far superior to the model that only 
# contains the predictor lstat. This is not surprising, since earlier we 
# saw evidence for non-linearity in the relationship between medv and 
# lstat. If we type then we see that when the lstat2 term is included in 
# the model, there is little discernible pattern in the residuals.

par(mfrow=c(2,2))
plot(lm.fit2)

# The following command produces a fifth-order polynomial fit:
lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)

# Log transformation
summary(lm(medv~log(rm),data=Boston))


# Qualitative Predictors --------------------------------------------------

names(Carseats)
# The predictor Shelveloc takes on three possible values, Bad, Medium, 
# and Good. Given a qualitative variable such as Shelveloc, R generates 
# dummy variables automatically. Below we fit a multiple regression 
# model that includes some interaction terms.
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

# The contrasts() function returns the coding that R uses for the 
# dummy variables.
attach(Carseats)
contrasts (ShelveLoc)

# R has created a ShelveLocGood dummy variable that takes on a value of 1 if 
# the shelving location is good, and 0 otherwise. It has also created 
# a ShelveLocMedium dummy variable that equals 1 if the shelving location 
# is medium, and 0 otherwise. A bad shelving location corresponds to a zero 
# for each of the two dummy variables. The fact that the coefficient for
# ShelveLocGood in the regression output is positive indicates that a good 
# shelving location is associated with high sales 
# (relative to a bad location). And ShelveLocMedium has a 
# smaller positive coefficient, indicating that a medium shelving 
# location leads to higher sales than a bad shelving location but 
# lower sales than a good shelving location.


# Functions ---------------------------------------------------------------

LoadLibraries = function (){
        library(ISLR)
        library(MASS)
        print ("The libraries have been")
}
LoadLibraries
LoadLibraries()
