# Generate data for non-linear class boundary
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, 2] - 2
y <- c(rep(1, 150), rep(2, 50))
dat = data.frame(x = x, y = as.factor(y))

# Plotting the data
plot(x, col = y) # indeed it is nonlinear

# The data is randomly split into training and testing groups.
# We then fit the training data using the svm() function with a radiaal kernel lambda = 1
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])

summary(svmfit)

# Increasing cost
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ]) # increasing cost results on irregular decision boundary

# Cross Validation
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train, ], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out) # best choice of parameters involves cost = 1, and gamma = 1

# Test set predictions
table(true = dat[-train, "y"], pred = predict(tune.out$best.model,
      newx = dat[-train, ])) # 34% (18+16/58+16+18+8) of test observations are misclassified by SVM



# ROC Curves --------------------------------------------------------------
library(ROCR)
rocplot <- function(pred, truth, ...) {
        predob = prediction(pred, truth)
        perf = performance(predob, "tpr", "fpr")
        plot(perf, ...)
}

# svm outputs the class labels. what if we want to output fitted values, i.e, response
svmfit.opt <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = TRUE))$decision.values # the sign of the fitted values determines the class label where it belong

# ROC plot (training)
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

svmfit.flex <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

# ROC plot (test)
fitted <- attributes(predict(svm.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red") # higher AUC more accurate predictions

