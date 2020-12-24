
# Data Setup --------------------------------------------------------------
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

# Best Subset Selection ---------------------------------------------------
regfit.best = regsubsets(Salary ~ ., data = Hitters[train,], nvmax =19)

# Compute the validation set error for the best model of each model size
test.mat = model.matrix(Salary ~ ., data = Hitters[test, ])

val.errors = rep(NA, 19)
for (i in seq_along(val.errors)) {
        coefi = coef(regfit.best, id = i)
        pred = test.mat[, names(coefi)] %*% coefi
        val.errors[[i]] = mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 19)

# Predict method
predict.regsubsets = function(object, newdata, id, ...) {
        form = as.formula(object$call[[2]])
        mat = model.matrix(form, newdata)
        coefi = coef(object, id = id)
        xvars = names(coefi)
        mat[, xvars] %*% coefi 
}

# The best ten-variable model on the full data set has a different set of
# variables than the best ten-variable model on the training set
regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best ,19)


# Compute the cross-validation set error for the best model of each 
# model size
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# Compute the test errors on the appropriate subset, and store them in the 
# appropriate slot in the matrix cv.errors
for (j in 1:k) {
        best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ],
                              nvmax = 19)
        for (i in 1:19) {
                pred = predict(best.fit, Hitters[folds == j, ], id = i)
                cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
        }
}
cv.errors
# This has given us a 10×19 matrix, of which the (i, j)th element corresponds 
# to the test MSE for the ith cross-validation fold for the best j-variable 
# model. We use the apply() function to average over the columns of this 
# matrix in order to obtain a vector for which the jth element is the 
# cross-validation error for the j-variable model.

mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors ,type="b")
# We see that cross-validation selects an 11-variable model. We now perform 
# best subset selection on the full data set in order to obtain the 
# 11-variable model.

reg.best=regsubsets (Salary ~ .,data=Hitters , nvmax=19)
coef(reg.best ,11)

