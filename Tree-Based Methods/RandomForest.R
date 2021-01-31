library(randomForest)
library(MASS)

# By default, randomForest() uses p/3 variables when building a 
# random forest of regression trees, and √p variables when building 
# a random forest of classification trees. Here we use mtry = 6.
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
rf.boston = randomForest(medv ~ ., data = Boston, subset = train,
                         mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, 'medv']
mean((yhat.rf - boston.test)^2)

# The test set MSE is 11.31; this indicates that random forests did not yield
# an improvement over bagging in this case.

# Using the importance() function, we can view the importance of 
# each variable.
importance(rf.boston)
varImpPlot (rf.boston)
# The results indicate that across all of the trees considered in 
# the random forest, the wealth level of the community (lstat) 
# and the house size (rm) are by far the two most important variables.
