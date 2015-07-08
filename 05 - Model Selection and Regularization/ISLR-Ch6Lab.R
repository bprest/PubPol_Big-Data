### ISLR Chapter 6 Lab
## Lab 1: Subset Selection Methods
#install.packages("ISLR")
library(ISLR)

names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))


#install.packages("leaps")
library(leaps)

# regsubsets(y~x, data=, nvmax=8) 
# regubsets() does an exhaustive search of the best fitting models.
# It fits alll possible 1 variable models, all possible 2 variable models, etc.
# up to all possible nvmax variable models (8 by default). It then reports what 
# the best fitting model is for each variable number, based on R^2.
regfit.full = regsubsets(Salary~., Hitters) 
summary(regfit.full)
# Best 1 variable model is CRBI
# Best 2 variable model is CRBI and Hits
# Best 3 variable model is CRBI, Hits, and PutOuts
regfit.full = regsubsets(Salary~., Hitters, nvmax = 19)  
# Hitters has 20 variables and 1 is Salary, leaving 19 RHS variables.
reg.summary = summary(regfit.full)

names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type="l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adj RSq", type="l")

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red", cex=2, pch=20)
# pch = plot char. cex = character expansion (size of points)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex=2, pch=20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex=2, pch=20)

# regsubsets() has special functionality with the plot() function.
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
# For each variable, lots of black boxes above it indicates it is important.
# Each "row" of boxes is a single regression. The height of the row indicates 
#  the value of the diagnostic plotted (e.g., R^2). The box is white if the
# variable written below it is NOT included in the model, and grey/black if it IS.
# In general, the best fitting models with lots of variables will be at the top,
# and the worse-fitting but simpler models will be at the bottom.
which.min(reg.summary$bic)
coef(regfit.full, 6) # find the coefficients from the 6th model (the one with the best BIC)

# This search was exhaustive and would take quite a long time with larger nvmax.
# We can make it do forward or backward selection using the method= argument.
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.fwd)
summary(regfit.bwd)
# First 6 choices are the same, but the 7th is different
coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# The above used R^2/BIC/Cp to choose models. But we can also do it by validation set.
# Here we do the validation set approach, splitting the sample into a test set and a training set.
set.seed(1)
train = sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test = !train

regfit.best = regsubsets(Salary~., data=Hitters[train,], nvmax=19)
test.mat = model.matrix(Salary~., data=Hitters[test,])
# model.matrix() builds an X matrix from the model and dataset specified.
class(test.mat)

# There's no "predict" or "residuals" function for regsubsets, so we do it by hand:
val.errors = rep(NA,19)
for(i in 1:19) {
        # find the beta hat coefficients from the ith equation
        coefi = coef(regfit.best, id=i)
        # grab the relevant coefficients from the test X matrix, and multiply by beta hat to get predicted values
        pred = test.mat[,names(coefi)] %*% coefi
        # Calculate test MSE
        val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
which.min(val.errors) 
coef(regfit.best,10)
# We find that the best test MSE comes from the 10 variable model. 
# So we fit the 10 variable model on the full dataset to get our coefficient estimates.
regfit.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best,10)
# But wait! The best 10 variable model on the full dataset has a slightly different
# set of coefficients than the 10 variable model from the training set!
# This is OK!

# We will want to do this MSE estimation again, so we write it into a function.
# The function will take object= a regsubsets fit. 
#  Note that such a fit will have an element named "call" containing the formula.
names(regfit.best) # elements of the model fit
regfit.best$call
names(regfit.best$call) # elements of $call
regfit.best$call[3]
regfit.best$call[[3]] # what's the logic here?

predict.regsubsets = function(object, newdata, id, ...) {
        form = as.formula(object$call[[2]]) #
        mat = model.matrix(form, newdata)
        coefi = coef(object, id=id)
        xvars = names(coefi)
        mat[,xvars] %*% coefi
}
# Note that the syntax predict.regsubsets does more than create a function.
# It creates a method WITHIN predict(), that allows it to take regsubsets as an object.


# Ok now let's redo the above with k-fold Cross-Validation
# Let's do k=10

k=10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors = matrix(NA,k,19, dimnames=list(NULL,paste(1:19)))
# kxp matrix containing the kth fold's test MSE with p variables.

# Loop through the k folds, fitting all 19 eqns on non-k obs.
for (j in 1:k){
        # Fit the regressions on non-k folds (the training set):
        best.fit = regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
        # Then, for each of the 19 equations, fit on the kth fold (the test set)
        for (i in 1:19) {
                # Method A: Call the function above just like you normally would:
                #pred = predict.regsubsets(object = best.fit, newdata = Hitters[folds==j,], id=i)
                # Method B: Use the fact that we made a new method for predict():
                pred = predict(object = best.fit, newdata = Hitters[folds==j,], id=i)
                # These two methods do the exact same thing
                # Now calculate teh test MSE
                cv.errors[j,i] = mean( (Hitters$Salary[folds==j]-pred)^2 )
        }
}
cv.errors
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')
which.min(mean.cv.errors) # 11th is best
reg.best = regsubsets(Salary~., data=Hitters, nvmax=19) # refit model
coef(reg.best,11)


## Lab 2: Ridge Regression and Lasso
rm(list=ls())
install.packages("glmnet")
library(glmnet)
Hitters = na.omit(Hitters)

x = model.matrix(Salary~., Hitters)[,-1] 
# note: model.matrix creates dummies, since glmnet requires a matrix as
# inputs (i.e., no strings or factors).
y = Hitters$Salary

grid = 10^seq(10,-2,length=100) 
# want to estimate our model with a range of lambda values, going from 10^-2 = 0.01 (basically zero) to 10^10
ridge.mod = glmnet(x, y, alpha=0, lambda=grid) # ridge regression
# the penalty term is lambda*[(1-alpha)/2*L2(beta) + alpha*L1(beta)]
# where L2() is the L-2 norm and L-1 is the L1 norm.
# Basically you can do a mix between Ridge (alpha=0) and Lasso (alpha=1).
# Note that glmnet automatically standardizes the variables. Can turn this off with standardize=FALSE
dim(coef(ridge.mod)) # KxL where L is the size of lambda. Contains all the parameter values for each lambda value.
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
ridge.mod$lambda[60]
coef(ridge.mod)[,60]

# glmnet has special predict methods
?predict.glmnet
# We can interpolate over current estimates to estimate the coefficients when lambda=50 (or any other value really):
predict(ridge.mod, s=50, type="coefficients")[1:20,]

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2) # randomly select half the sample
test = -train

y.test = y[test]
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)
# This is much lower than had we not included any covariates at all.
# Had we not included any covariates (i.e., just an intercept), the predicted value would just be the mean y[train]
mean((mean(y[train])-y.test)^2)
# This would give the same result as setting lambda=+infty (so all coefficients are zero)
ridge.pred = predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred = predict(ridge.mod, s=0, newx=x[test,], exact=TRUE) # exact=TRUE refits the model, rather than interpolating
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type="coefficients")[1:20,]

# But what lambda is best? We figure out by using cross-validation techniques, within the training set
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0) # by default performs 10-fold CV
plot(cv.out)
bestlam = cv.out$lambda.min

ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred-y.test)^2)

## Now that we have the best lambda value, we fit the model on the full data set.

out = glmnet(x,y,alpha=0) # note: if you don't supply a lambda, it creates it itself
predict(out, s=bestlam, type="coefficients")[1:20,]

## Lasso
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
coef(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)

out = glmnet(x,y,alpha=1, lambda=grid) # refit full model
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:20,] # get coefficients with optimal lambda
lasso.coef # many variables == 0
lasso.coef[lasso.coef!=0] # only 7 variables

### Lab 3: Prinicpal Components Regression and Partial Least Squares
# Principal Components Regression (PCR)
#install.packages("pls")
library("pls")
set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
# scale = TRUE standardizes variables
# validation = "CV" instructs pcr() to do 10-fold cross validation to find the optimal number of principal components
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")
# Lowest is a M=16, but it's not much better than M=1.
# The above was a CV on the full data. Now let's do it on a training set only,
# which we will then compare to the test set.

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
# Now lowest is M=7. Use this and compute a test MSE:
pcr.pred = predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred-y.test)^2)

# Now fit on full model using 7 components:
pcr.fit = pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

# Partial Least Squares (PLS)
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")
# M=2
pls.pred = predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit = plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)
# PLS finds a simpler model than PCR. This is because PCR picks the components
# that fit the X variables the best, while PLS picks components that are important
# for both X and Y jointly.
