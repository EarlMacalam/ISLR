

# Data --------------------------------------------------------------------

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9]) # error when a variable is qualitative

# As one would expect, the correlations between the lag variables and today’s 
# returns are close to zero. In other words, there appears to be little 
# correlation between today’s returns and previous days’ returns. The only 
# substantial correlation is between Year and Volume. By plotting the data
# we see that Volume is increasing over time. In other words, the average 
# number of shares traded daily increased from 2001 to 2005.
attach(Smarket)
plot(Volume)


# Logistic Regression -----------------------------------------------------

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket ,family=binomial)
summary(glm.fit)

# The smallest p-value here is associated with Lag1. The negative coefficient 
# for this predictor suggests that if the market had a positive return 
# yesterday, then it is less likely to go up today. However, at a value 
# of 0.15, the p-value is still relatively large, and so there is no clear 
# evidence of a real association between Lag1 and Direction.


# Prediction --------------------------------------------------------------

# The type="response" option tells R to output probabilities 
# of the form P(Y = 1|X)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts (Direction )
# Here we have printed only the first ten probabilities. We know that 
# these values correspond to the probability of the market going up, 
# rather than down, because the contrasts() function indicates that R 
# has created a dummy variable with a 1 for Up.

glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
The first command creates a vector of 1,250 Down elements. The second line 
transforms to Up all of the elements for which the predicted probability of 
a market increase exceeds 0.5. 

# Given these predictions, the table() function 
# can be used to produce a confusion matrix in order to determine how many 
# observations were correctly or incorrectly classified.
table(glm.pred,Direction)
mean(glm.pred==Direction )
# The diagonal elements of the confusion matrix indicate correct predictions, 
# while the off-diagonals represent incorrect predictions. Hence our model 
# correctly predicted that the market would go up on 507 days and that 
# it would go down on 145 days, for a total of 507 + 145 = 652 correct 
# predictions. The mean() function can be used to compute the fraction of 
# days for which the prediction was correct. In this case, logistic regression 
# correctly predicted the movement of the market 52.2 % of the time.


# Assessing the model by splitting into test and training -----------------
# Splitting the data
train=(Year <2005)
Smarket.2005 = Smarket [! train ,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

## Model ##

# fit a logistic regression model using only the subset of the observations 
# that correspond to dates before 2005, using the subset argument.
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , 
            data=Smarket ,family=binomial,subset=train)

# obtain predicted probabilities of the stock market going up for each of 
# the days in our test set—that is, for the days in 2005.
glm.probs=predict(glm.fit,Smarket.2005,type="response")
# Notice that we have trained and tested our model on two completely separate 
# data sets: training was performed using only the dates before 2005, 
# and testing was performed using only the dates in 2005. 


# Finally, we compute the predictions for 2005 and compare them to the 
# actual movements of the market over that time period.
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
# The != notation means not equal to, and so the last command computes the 
# test set error rate. The results are rather disappointing: the test error 
# rate is 52%, which is worse than random guessing!

# We recall that the logistic regression model had very underwhelming p-values 
# associated with all of the predictors, and that the smallest p-value,
# though not very small, corresponded to Lag1. Perhaps by removing the 
# variables that appear not to be helpful in predicting Direction, we can 
# obtain a more effective model. After all, using predictors that have no 
# relationship with the response tends to cause a deterioration in the test 
# error rate (since such predictors cause an increase in variance without
# a corresponding decrease in bias), and so removing such predictors may 
# in turn yield an improvement. Below we have refit the logistic regression 
# using just Lag1 and Lag2, which seemed to have the highest predictive power 
# in the original logistic regression model.
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket ,family=binomial, subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
# Now the results appear to be a little better: 56% of the daily movements have
# been correctly predicted. It is worth noting that in this case, a much
# simpler strategy of predicting that the market will increase every day
# will also be correct 56% of the time! Hence, in terms of overall error rate,
# the logistic regression method is no better than the naive approach. 
# However, the confusion matrix shows that on days when logistic regression
# predicts an increase in the market, it has a 58% accuracy rate. This 
# suggests a possible trading strategy of buying on days when the model
# predicts an increasing market, and avoiding trades on days when a decrease
# is predicted.

predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)),
        type="response")
# the probability that the direction is up is 0.4791462 and 0.4960939.