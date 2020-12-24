set.seed(1)
pls.fit=plsr(Salary~., data=Hitters ,subset=train,scale=TRUE, validation ="CV")
summary(pls.fit)
# The lowest cross-validation error occurs when only M = 1 partial least 
# squares directions are used. We now evaluate the corresponding test set MSE.
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

# Finally, we perform PLS using the full data set, using M = 1, 
# the number of components identified by cross-validation.
pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE,ncomp=1)
summary(pls.fit)
