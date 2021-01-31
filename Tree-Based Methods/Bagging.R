library(randomForest)
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
bag.boston = randomForest(medv ~ ., data = Boston, subset = train,
                          mtry = 13, importance = TRUE)
bag.boston
# mtry is the number of variables that should be consider for 
# every split; mportance is the feature where it got to show 
# the variable's worth.

# How well does this bagged model perform on the test set?
yhat.bag = predict(bag.boston, newdata = Boston[-train, ]) 
boston.test = Boston[-train, 'medv']
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - bosten.test)^2)
# The test set MSE associated with the bagged regression tree is 16.0594.

# We could change the number of trees grown by randomForest() 
# using the ntree argument:        
bag.boston = randomForest(medv ~ ., data = Boston, subset = train,
                          mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train, ])
mean((yhat.bag - bosten.test)^2)        
        
        
        

