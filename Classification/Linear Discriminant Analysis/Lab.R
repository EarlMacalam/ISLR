
# Linear Discriminant Analysis --------------------------------------------
library(MASS)
library(ISLR)

names(Smarket)
attach(Smarket)
train=(Year <2005)
Smarket.2005 = Smarket [! train ,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
lda.fit
# The LDA output indicates that πˆ = 0.492 and πˆ = 0.508; in other words,
# 49.2% of the training observations correspond to days during which the 
# market went down. It also provides the group means; these are the average
# of each predictor within each class, and are used by LDA as estimates of 
# μk. These suggest that there is a tendency for the previous 2 days’ returns 
# to be negative on days when the market increases, and a tendency for the 
# previous days’ returns to be positive on days when the market declines.


# The predict() function returns a list with three elements. The first element, 
# class, contains LDA’s predictions about the movement of the market. 
# The second element, posterior, is a matrix whose kth column contains the 
# posterior probability that the corresponding observation belongs to the kth 
# class.Finally, x contains the linear discriminants, described earlier.
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class ,Direction.2005)
mean(lda.class==Direction.2005)


# Quadratic Discriminant Analysis -----------------------------------------
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
qda.fit
# The output contains the group means. But it does not contain the coefficients 
# of the linear discriminants, because the QDA classifier involves a 
# quadratic, rather than a linear, function of the predictors. The predict() 
# function works in exactly the same fashion as for LDA.

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class ,Direction.2005)
mean(qda.class==Direction.2005)
# Interestingly, the QDA predictions are accurate almost 60% of the time, 
# even though the 2005 data was not used to fit the model. This level of 
# accuracy is quite impressive for stock market data, which is known to be 
# quite hard to model accurately. This suggests that the quadratic form 
# assumed by QDA may capture the true relationship more accurately than 
# the linear forms assumed by LDA and logistic regression. However, we 
# recommend evaluating this method’s performance on a larger test set 
# before betting that this approach will consistently beat the market!
