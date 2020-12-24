library(pls)
set.seed(2)
pcr.fit=pcr(Salary ~ ., data=Hitters ,scale=TRUE, validation = "CV")
# Setting scale=TRUE has the effect of standardizing each predictor, 
# using (6.6), prior to generating the principal components, so that the 
# scale on which each variable is measured will not have an effect. Setting 
# validation="CV" causes pcr() to compute the ten-fold cross-validation error 
# for each possible value of M , the number of principal components used. 
# The resulting fit can be examined using summary().
summary(pcr.fit)
# Note that pcr() reports the root mean squared error; 
# in order to obtain the usual MSE, we must square this quantity. 

# plot the cross-validation scores 
validationplot(pcr.fit,val.type="MSEP")

# We now perform PCR on the training data and evaluate its test 
# set performance
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE,
              validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
# the lowest cross-validation error occurs when M = 5 component are used.
# We compute the test MSE as follows.
pcr.pred=predict(pcr.fit,x[test,],ncomp=5)
mean((pcr.pred-y.test)^2)

# Finally, we fit PCR on the full data set, using M = 5, the 
# number of components identified by cross-validation.
pcr.fit=pcr(y~x,scale=TRUE,ncomp=5)
summary(pcr.fit)











