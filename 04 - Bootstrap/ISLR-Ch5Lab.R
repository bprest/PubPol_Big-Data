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
# LOOCV can be done automatically with any glm() model




