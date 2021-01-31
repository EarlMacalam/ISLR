library(tree)
library(ISLR)

attach(Carseats)
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

# Response Variable should be factor for categorical trees
Carseats$High = as.factor(Carseats$High)
str(Carseats)

# Using the tree() function to fit a classification tree in order to 
# predict High using all variables but Sales
tree.carseats = tree(High ~ .-Sales, Carseats)
summary(tree.carseats) # We see that the training error rate is 9 %.
                       # A small deviance indicates a tree that provides a good fit to the (training) data

# Plots
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.carseats

# We split the observations into a training set and a test set, build the 
# tree using the training set, and evaluate its performance on the test data.
set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = High[-train]

tree.carseats = tree(High ~ .-Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class") # the argument type ="class" instructs R to return the actual class prediction
table(tree.pred ,High.test)
(104 + 50) / 200 # This approach leads to correct predictions for around 77 % of the locations in the test data set


# Tree Pruning ------------------------------------------------------------
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# Note that, despite the name, dev corresponds to the cross-validation error 
# rate in this instance. The tree with 8 terminal nodes results in the lowest 
# cross-validation error rate, with 75 cross-validation errors.

# Plot the error rate as a function of both size and k
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Apply the prune.misclass() function in order to prune the tree to
# obtain the 8-node tree
prune.carseats = prune.misclass(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# How well does this pruned tree perform on the test data set?
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(89 + 62) / 200 # Now 77.5% of the test observations are correctly classified, so not only has the pruning process produced a more interpretable tree, but it has also improved the classification accuracy.

# (If we increase the value of best, we obtain a larger pruned tree with 
# lower classification accuracy) But here in my results we still obtain 
# the same proportion of accuracy.
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(102 + 53) / 200















