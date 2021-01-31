library(gbm)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
boost.boston = gbm(medv ~ ., data = Boston[-train, ],
                   distribution = 'gaussian', n.trees = 5000,
                   interaction.depth = 4)
# We run gbm() with the option distribution="gaussian" since this 
# is a regression problem; if it were a binary classification 
# problem, we would use distribution="bernoulli". The argument 
# n.trees=5000 indicates that we want 5000 trees, and the option 
# interaction.depth=4 limits the depth of each tree.

summary(boost.boston)
# We see that lstat and rm are by far the most important variables

# We can also produce partial dependence plots for these two 
# variables. These plots illustrate the marginal effect of the 
# selected variables on the response after integrating out the 
# other variables. In this case, as we might expect, median house 
# prices are increasing with rm and decreasing with lstat.
par(mfrow = c(1, 2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')

# Prediction --------------------------------------------------------------
yhat.boost = predict(boost.boston, newdata = Boston[-train,],
                     n.trees = 5000)
boston.test = Boston[-train, 'medv']
mean((yhat.boost - boston.test)^2)

# The test MSE obtained is 1.67332e-06. This is superior compared
# to bagging and random forest. 

# If we want to, we can perform boosting with a different value of 
# the shrinkage parameter λ in (8.10). The default value is 0.001, 
# but this is easily modified. Here we take λ = 0.2.
boost.boston = gbm(medv ~ ., data = Boston[train, ],
                   distribution = 'gaussian', n.trees = 5000,
                   interaction.depth = 4, shrinkage = 0.2,
                   verbose = F)
yhat.boost = predict(boost.boston, newdata = Boston[-train, ],
                     n.trees = 5000)        
mean((yhat.boost - boston.test)^2)
# In this case, using λ = 0.2 leads to a higher test MSE than λ = 0.001.
