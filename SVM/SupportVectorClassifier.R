# Generating observations that belong to two classes
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 1

# Are the classes linearly separable? (They are not)
plot(x, col = (3 - y))

# Encode response as factor variable
dat <- data.frame(x = x, y = as.factor(y))
library(e1071)
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE) # arg scale means scale data to mean 0 and stdv 1

# Plot
plot(svmfit, dat) # the support vectors are plotted as crosses while normal observations are circles. In this case we have 7 support vectors

# Dertermining support vectors obs
svmfit$index

# Summary
summary(svmfit) # 4 sv in one class and 3 in the other

# Smaller value of the cost parameter
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index

# Cross-validation
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)        

bestmod <- tune.out$best.model       
summary(bestmod)        

# Generating test data
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

# Predicting the class labels of test obeservations
# We use the best model obtained from cross-validation
ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y) # with this value of cost, 17 observationss are correctly classified

# What if cost = 0.01?
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y) # additional observation is misclassified


















