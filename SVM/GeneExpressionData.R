library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# we have 60 obs for training and 20 obs for test. also we have 2308 features
table(Khan$ytrain)
table(Khan$ytest)

# there are a very large number of features relative to the number of observations.
# this suggests that we should use a linear kernel, because the additional flexibility that will result from using a polynomial or radial kernel is unnecessary
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y) # no training errors, because the large number of variables implies that it is easy to find hyperplanes that fully separate the classes

# On test data
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)

