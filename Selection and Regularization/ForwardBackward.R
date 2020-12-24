
# Forward Stepwise Selection ----------------------------------------------
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19,
                        method = "forward")
summary(regfit.fwd)


# Backward Stepwise Selection ---------------------------------------------
regfit.bwd = regsubsets (Salary ~ ., data = Hitters, nvmax = 19,
                         method = "backward")
summary(regfit.bwd)

# the best seven-variable models identified by forward stepwise selection,
# backward stepwise selection, and best subset selection are different.
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

