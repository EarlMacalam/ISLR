library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston = tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

# Plot
plot(tree.boston)
text(tree.boston, pretty = 0)

# The variable lstat measures the percentage of individuals with lower 
# socioeconomic status. The tree indicates that lower values of lstat 
# correspond to more expensive houses. Also, higher average number 
# of rooms per dwelling in a suburb correspond to more expensive houses.
# Houses with high crime rate and high percentage of individuals with 
# socioeconomic status have low price. (rm>=6.9595, lstat>4.405, and crim>11.4863)


# Tree Pruning ------------------------------------------------------------
cv.boston = cv.tree(tree.boston)
names(cv.boston)
cv.boston
plot(cv.boston$size, cv.boston$dev, type = 'b')

# In this case, the most complex tree is selected by cross-validation (unpruned tree)
prune.boston_cv = prune.tree(tree.boston, best = 7)
plot(prune.boston_cv)
text(prune.boston_cv, pretty = 0)

# However, if we wish to prune the tree, we could do so as follows, 
#using the prune.tree() function
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
        
# In keeping with the cross-validation results, we use the unpruned tree to 
# make predictions on the test set
y_hat = predict(prune.boston_cv, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(y_hat, boston.test)
abline(0, 1)
mean((y_hat - boston.test)^2)
# In other words, the test set MSE associated with the regression tree 
# is 35.29. The square root of the MSE is therefore around 5.941, 
# indicating that this model leads to test predictions that are within 
# around $5, 941 of the true median home value for the suburb.
