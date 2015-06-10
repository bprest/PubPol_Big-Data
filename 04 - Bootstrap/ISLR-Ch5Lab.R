## Brian Prest
## 6/10/2015
## Intro to Statistical Learning: Chapter 5 Lab: Cross-Validation and Bootstrap
library(ISLR)
head(Auto)

### Validation Set
set.seed(1)
train = sample(392,196) # generates a set of size=196 random numbers between 1 and x=392. "sample(x = 392, size = 196)"
# by default, samples without replacement ("replace=FALSE") (fine here: we're splitting the data into 2)
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) # test set MSE = 26.14

lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2) # test set MSE = 19.82

lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2) # test set MSE = 19.78

# So cubic was better than linear or quadratic. But some of this is due to the random sample we drew:
# If we set the seed = 2 it gives slightly different results: 23.3, 18.9, 19.3. Now quadratic is better.

### Leave-One-Out Cross-validation (LOOCV)
# LOOCV can be done automatically with any glm() model (or cv.glm...cross-validation of generalized linear models)

glm.fit = glm(mpg~horsepower, data=Auto) # by default does a standard linear regressoin (OLS)
coef(glm.fit)
summary(lm(mpg~horsepower, data=Auto)) # Check: that this is indeed the case

library(boot) # Bootstrap resampling library
# Step 1: Fit the model
glm.fit = glm(mpg~horsepower, data=Auto) # by default does a standard linear regressoin (OLS)
# Step 2: Apply the cv.glm function
cv.err = cv.glm(Auto, glm.fit) 
names(cv.err) # returns the function that called it, the K chosen, "delta" which is the MSE
cv.err$call #
cv.err$K # =N=392
cv.err$delta # Cross-Validation results. = Average MSE in the K-fold cross-validation. 
# First is raw MSE, second is adjusted for bias (recall that LOOCV has low bias but high variance. Low bias means there is little correction needed)
cv.err$seed # RNG Seed when it was called.

# cv.glm does a K-fold cross validation. Inputs are data= DATASET; glmfit = FITTEDGLM; and K = NUMFOLDS. By default, K = N, so it does the LOOCV method
#options(digits=4) # significant digits

# Let's see how the CV scores different degrees of polynomial.
cv.error = rep(0,5)
for(i in 1:5) {
        glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
        cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error # reveals that the minimum MSE is at a 2nd degree polynomial

### K-Fold CV
set.seed(17)
cv.error.10 = rep(0,10) # 10 polynomials, K=10
for (i in 1:10) {
        glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
        cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10 # best is now at 5th degree, but not by much. Most of the gains occur at the 2nd polynomial.
round(cv.error.10,2)
#format(round(cv.error.10,2), nsmall=2)

# With K-fold CV, we have lower noise in the MSE estimate, but more bias (compared to LOOCV). Hence, $delta[2] is a biased corrected estimate.

### Bootstrap
# Step 1: Create a function that estimates the relevant statistic
head(Portfolio)
alpha.fn = function(data,index) {
        X = data$X[index]
        Y = data$Y[index]
        return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:nrow(Portfolio))
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T)) 
# in principle we could loop through this function 1,000 times. Or just use the boot() function
alpha.bs = boot(data = Portfolio, statistic = alpha.fn, R=1000) 
# the statistic argument MUST be a function where the first argument is the data and the second is an index.
alpha.bs
names(alpha.bs)
bias = alpha.fn(Portfolio,1:nrow(Portfolio)) - mean(alpha.bs$t)
stderr = sqrt(var(alpha.bs$t))

boot.fn = function(data,index) return(coef(lm(mpg~horsepower, data=data, subset=index)))

boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))

boot(data=Auto, statistic=boot.fn, R=1000)
# Compare to the analytic form of the standard errors from summary(lm())
summary(lm(mpg~horsepower, data=Auto))$coef 
# different. bootstrap has higher std errors. 
# why? Because the analytic estimate assumes that the linear model is correct (to get the MSE), 
# and we have clearly shown a nonlinear relationship. 
# The bootstrap doesn't use this assumption, and therefore gets a better estimate.
# If we use a quadratic form, we should hopefully get similar results between 
# the bootstrap and the analytic formulas
#boot.fn2 = function(data,index) return(coef(lm(mpg~poly(horsepower,2), data=data, subset=index)))
boot.fn2 = function(data,index) return(coef(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
set.seed(1)
boot(data=Auto, statistic=boot.fn2, R=1000)

#summary(lm(mpg~poly(horsepower,2), data=Auto))$coef
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef 




