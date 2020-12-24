x = model.matrix(Salary ~ ., Hitters)[,-1]
y = Hitters$Salary


library(glmnet)
grid = 10^seq(10,-2,length=100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
# Associated with each value of λ is a vector of ridge regression coefficients, 
# stored in a matrix that can be accessed by coef(). In this case, it is a 
# 20×100glmnet()matrix, with 20 rows (one for each predictor, plus an intercept) 
# and 100 columns (one for each value of λ).
dim(coef(ridge.mod))

# coefficients when λ = 11,498, along with their l2 norm
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))                

# here are the coefficients when λ = 705, along with their l2 norm.
# Note the much larger l2 norm of the coefficients associated with this
# smaller value of λ.
ridge.mod$lambda [60] 
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# We can use the predict() function for a number of purposes. For instance, 
# we can obtain the ridge regression coefficients for a new value of λ, say 50
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# splitting the samples into a training set and a test set in order to 
# estimate the test error of ridge regression and the lasso
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test=y[test]

# Next we fit a ridge regression model on the training set, and evaluate 
# its MSE on the test set, using λ = 4. Note the use of the predict() 
# function again. This time we get predictions for a test set, by 
# replacing type="coefficients" with the newx argument.
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# The test MSE is 142199.2. Note that if we had instead simply fit a model 
# with just an intercept, we would have predicted each test observation
# using the mean of the training observations. In that case, we could
# compute the test set MSE like this
mean((mean(y[train])-y.test)^2)

# We could also get the same result by fitting a ridge regression model with
# a very large value of λ. Note that 1e10 means 10^10
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# So fitting a ridge regression model with λ = 4 leads to a much lower test
# MSE than fitting a model with just an intercept. 


# We now check whether there is any benefit to performing ridge 
# regression with λ = 4 instead of just performing least squares 
# regression. Recall that least squares is simply 0
ridge.pred=predict(ridge.mod,s=0,newx=x[test,]) 
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,type="coefficients")[1:20,]

# In general, instead of arbitrarily choosing λ = 4, it would be better to 
# use cross-validation to choose the tuning parameter λ. We can do this 
# using the built-in cross-validation function, cv.glmnet(). By default, 
# the function performs ten-fold cross-validation, though this can be 
# changed using the argument nfolds.
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# Therefore, we see that the value of λ that results in the smallest 
# cross-validation error is 326. What is the test MSE associated with 
# this value of λ?
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)

# This represents a further improvement over the test MSE that we got 
# using λ = 4. Finally, we refit our ridge regression model on the full 
# data set, using the value of λ chosen by cross-validation, and examine 
# the coefficient estimates.
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# As expected, none of the coefficients are zero—ridge regression 
# does not perform variable selection.

























































