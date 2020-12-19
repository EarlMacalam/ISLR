
# The Validation Set Approach ---------------------------------------------

library(ISLR)
set.seed (1)
train = sample(392,196)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
# Therefore, the estimated test MSE for the linear regression fit is 23.26601

# Quadratic Regression
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
# Error rate: 18.71646

# Cubic Regression
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
# Error rate: 18.79401

# Choosing a different training set
set.seed (2)
train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
# Error rate: 25.72651

# Quadratic Regression
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
# Error rate: 20.43036

# Cubic Regression
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
# Error rate: 20.38533

# A model that predicts mpg using a quadratic function of horsepower performs 
# better than a model that involves only a linear function of horsepower, and 
# there is little evidence in favor of a model that uses a cubic function of 
# horsepower.

# Leave-One-Out Cross-Validation ------------------------------------------
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
# Our cross-validation estimate for the test error is approximately 24.23

# Regression up to 5th order
cv.err = vector("double", 5)
for (i in seq_along(cv.err)) {
        glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
        cv.err[[i]] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.err

# We see a sharp drop in the estimated test MSE between the linear and 
# quadratic fits, but then no clear improvement from using higher-order 
# polynomials.

# k-Fold Cross-Validation -------------------------------------------------
set.seed(17)
cv.error.10 = vector("double", 10)
for (i in seq_along(cv.error.10)){
        glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
        cv.error.10[[i]] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# The two numbers associated with delta are essentially the same when LOOCV is 
# performed. When we instead perform k-fold CV, then the two numbers associated 
# with delta differ slightly. The first is the standard k-fold CV estimate.
# The second is a bias-corrected version. On this data set, the two estimates 
# are very similar to each other.


# The Bootstrap -----------------------------------------------------------

# Performing a bootstrap analysis in R entails only two steps. First, we 
# must create a function that computes the statistic of interest. Second, 
# we use the boot() function, which is part of the boot library, to perform 
# the bootstrap by repeatedly sampling observations from the data set with 
# replacement.

# Estimating the Accuracy of a Statistic of Interest
alpha.fn = function(data,index) {
        X = data$X[index]
        Y = data$Y[index]
        return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio ,1:100)

# 1 Bootstrap with one estimate
set.seed (1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

# 1000 data w/ 1000 estimates
boot(Portfolio, alpha.fn, R = 1000)
# The final output shows that using the original data, αˆ = 0.5758, 
# and that the bootstrap estimate for SE(αˆ) is 0.0886.

# Estimating the Accuracy of a Linear Regression Model
boot.fn = function(data, index) {
        return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

# 2 bootstraps with 2 estimates
boot.fn(Auto, sample(392, 392, replace = TRUE))
boot.fn(Auto, sample(392, 392, replace = TRUE))

# 1000 data w/ 1000 estimates
boot(Auto, boot.fn, 1000)

# Standard estimates
summary(lm(mpg ~ horsepower, data = Auto))$coef

# The bootstrap approach does not rely on the assumptions of linear 
# regression model, and so it is likely giving a more accurate estimate of the 
# standard errors of beta_0 and beta_1 than is the summary() function.

# Fitting the quadratic model to the data.
boot.fn = function(data,index) {
        coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data,
                        subset = index))
}

set.seed (1)
boot(Auto ,boot.fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef

# Since this model provides a good fit to the data there is now a better
# correspondence between the bootstrap estimates and the standard estimates
# of SE(β0), SE(β1) and SE(β2).
