### ISLR Chapter 3 (Lab 2): Linear Regression
rm(list=ls())

#install.packages("ISLR")
#install.packages("car")

library(MASS)
library(ISLR)

head(Boston)
summary(Boston)
names(Boston)

lm(medv~lstat) # error! medv and lstat are not objects, so it won't run
attach(Boston)

lm(medv~lstat) # now it works
lm(medv~lstat - 1) # no constant (idea is that it automatically includes a constant, a 1, as an additional variable. -1 eliminates that "1")

# A better way is to pass the full data to the lm() function, and tell it what model to run:
lm.fit = lm(medv~lstat, data=Boston)
summary(lm.fit)

# Side Note: the name "lm.fit" is arbitrary. There's nothing special about using this name. For example, we could have used the name "briansmodel":
briansmodel = lm(medv~lstat, data=Boston)
summary(briansmodel)

# So we ran the model, but we want more information (parameter estimates, standard errors, p-values, etc.)
lm.fit # gives us something, but not much
summary(lm.fit) # gives us much more information


# What things are saved in lm.fit when we run the model?
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit)
residuals(lm.fit)

# It will be useful to find a way to extract the regression results from the summary table:
results = coef(summary(lm.fit)) # this is a MATRIX, so we can grab things from it in the usual ways:
results
results[,1]
beta_hat = results[,"Estimate"] # same thing
results[,2]
SE = results[,"Std. Error"] # same thing

# Predict at certain values of the explanatory variable:
# Could do it "by hand":

beta_hat[1] + beta_hat[2]*5
beta_hat[1] + beta_hat[2]*10
beta_hat[1] + beta_hat[2]*15
# Simpler and cleaner: use the predict command
predict(lm.fit, data.frame(lstat=c(5,10,15)))
# It also allows you to get confidence intervals and prediction intervals on the fitted values:
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")
# Recall the difference: 
# The confidence interval is how close we are getting to the right model (ignoring the error term) (reducible error).
# The prediction interval relates to the stuff outside the model (the stuff in the error) (irreducible error)
# We can have very good estimates of the parameters (tight confidence interval), but still have our model explain only a small part of the variation in Y (large prediction interval)

plot(lstat,medv)
abline(lm.fit) 
# NOT:
abline(a=10, b=1) # line with formula: y = a + bx. Hence "a,b line"
abline(a=50, b=-1, lwd=5, col="orange") # line width = 5. For a list of colors, type "colors()"
plot(lstat,medv, pch="+") # Use the Plot CHaracter option to use + signs instead of circles. You can do whatever character you want here (try some)
# Note: plot makes a NEW plot. To add things to an existing plot, you have to use options like line() or points()
plot(1:25,1:25, pch=1:25) # See the basic options



plot(lm.fit) 
# By default, plots like of diagnostics: residual vs fitted, Q-Q plot, residuals vs leverage. Does so 1 graph at a time. 
# Want to see them all at once: Let's set up a table of 4 graphs. We set the graph PARameters using the par() function:
par(mfrow=c(2,2)) # mfrow standards for a multi-figure row of graphs
plot(lm.fit) 
par(mfcol=c(2,2)) # mfcol does the same thing, but instead of filling the panels by rows, it fills them by columns
plot(lm.fit) 

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit)) # hat values tell are indicators of leverage
which.max(hatvalues(lm.fit)) # tells us which observation is the max


# Multiple regression
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~., data=Boston) # use all variables in the Boston dataset
names(summary(lm.fit))

summary(lm.fit)$sigma
library(car) # The car package has some extra statistical tools, such as the VIF. Car stands for Companion to Applied Regression
vif(lm.fit)
# The VIF is a measure of multicollinearity. High levels of multicollinearity make it hard to precisely estimate variables. In other words, it inflates the variance of the estimates.
# Recall from the text (pg 101, 116 of pdf) that VIF>=1. Rule of thumb is that a VIF greater than 5 or 10 is a problem. Here we're typically ok.

# Let's drop age from the regression, since it is very small and insignificant.
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)

lm.fit1 = update(lm.fit,~.-age)

## Interactions

summary(lm(medv~lstat*age, data=Boston)) # interaction, including standalone variables
summary(lm(medv~lstat:age, data=Boston)) # interaction, excluding standalone variables


## Polynomials
# Option A: Do it by hand
lstat2 = Boston$lstat^2
summary(lm(medv~lstat+lstat2, data=Boston))
summary(lm(medv~lstat+I(lstat^2), data=Boston)) # I() is the Inhibit function. It inhibits R from interpreting the ^ as code for lm() to interpret, and instead carries out the operation
# Another example: suppose we wanted to run another regression where we forced ptratio and lstat to have the same coefficient. We would do this by adding the two variables together, and then treating their sum a single explanatory variable
summary(lm(medv~(lstat+ptratio), data=Boston)) # this didn't work
summary(lm(medv~I(lstat+ptratio), data=Boston)) # but this did
summary(lm(medv~lstat+I(lstat^2), data=Boston)) # The coefficient on the squared term is significant. This suggests that it helps the model.

# Formally test if it really helps the model: ANOVA test. H_o: Models equally good. H_a: More flexible model is significantly better
anova(lm(medv~lstat, data=Boston),lm(medv~lstat+I(lstat^2), data=Boston))
# p-value is basically zero => quadratic is better.

#
lm.fit2 = lm(medv~lstat+I(lstat^2), data=Boston)
plot(lstat,medv)
max(lstat)
x = seq(from=0, to=38, by=0.1)
y = predict(lm.fit2, data.frame(lstat=x))

lines(x,y, col="blue")
# Looks much better

par(mfrow=c(2,2))
plot(lm.fit2) # no pattern to the residuals


# But what if we want to do longer polynomials? Like a 5th order. It would be a pain to write out all 5 terms. Luckily, we can do this much easier with:
summary(lm(medv~poly(lstat, degree=2), data=Boston)) # IMPORTANT NOTE!!!! By default this uses Orthogonal Polynomials (like Chebyshev). If you want to force it to use raw polynomials, you must set raw=TRUE
summary(lm(medv~poly(lstat, degree=2, raw=T), data=Boston))
# Other transformations also work
summary(lm(medv~sqrt(lstat), data=Boston)) # square root
summary(lm(medv~log(lstat), data=Boston)) # natural log
log(exp(3))
log(100, base=10)

### Qualatative Predictors
library(ISLR)
summary(Carseats)
head(Carseats)
names(Carseats)
attach(Carseats)

# ShelveLoc is a 3-way factor variable, indicating whether the carseat is placed in a good, bad, or medium location.
summary(ShelveLoc)

# R automatically turns it into dummy variables.
summary(lm(Sales~ShelveLoc, data=Carseats)) # Note the dummy variable trap
summary(lm(Sales~ShelveLoc-1, data=Carseats))
summary(Sales[ShelveLoc=="Bad"])
summary(Sales[ShelveLoc=="Medium"])
summary(Sales[ShelveLoc=="Good"])

summary(lm(Sales~.+Income:Advertising+Price:Age, data=Carseats))
contrasts(ShelveLoc) # Shows how the dummy variables were created.


### Writing your own functions
LoadLibraries = function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded. Hurray!!")
}

# This is a function. If we just type in its name, it shows us the code underlying it. 
LoadLibraries
# To actually call the function, we need to use (), and give it an argument if appropriate
LoadLibraries()

# Some functions can be written in one line:
simplefunc = function(x,y) 2*x+y
simplefunc(x=2,y=3)

# If you want to write a more complicated function, it can take many lines:
briansfunction = function(x,y) {
  z = 2*x+y
  message = paste("The answer is", z)
  print(message)
  output = list(x,y,z,message)
  names(output) = c("x","y","z","message")
  return(output)
}


briansfunction(2,3)
briansfunction(x=2,y=3)
result = briansfunction(y=3,x=2)
# Note that none of the variables from the function are saved. Only the thing return()'ed gets saved. This can help save on memory.
output # doesn't work
z # also doesn't work



