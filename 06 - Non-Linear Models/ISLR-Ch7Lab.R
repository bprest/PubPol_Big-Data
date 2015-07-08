### ISLR Chapter 7 Lab
library(ISLR)
attach(Wage)

fit = lm(wage~poly(age,4), data=Wage) # by default, uses orthogonalized polynomials
coef(summary(fit))

fit2 = lm(wage~poly(age,4, raw=T), data=Wage)
coef(summary(fit2))

# Note: use of orthogonalized polynomials does not affect fitted values:
mean(predict(fit))
mean(predict(fit2))

# Can also create polynomials by hand
fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage) # using the wrapper function I() to interpret ^ literally
fit2b = lm(wage~cbind(age,age^2,age^3,age^4), data=Wage) # or by creating a matrix of the variables we want

# Let's plot the function we have estimated across a grid of age
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata=list(age = age.grid), se=T)
# note: predict() returns a list with fitted values ($fit), the stand error of the fitted values ($se.fit), and other stuff.
str(preds)
se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)

par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0)) # MARgins, Outer MArgins
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey") # scatter of data
title("Degree -4 Polynomial", outer=T)
lines(age.grid, preds$fit,lwd=2, col=2)

# Is the 4 degree polynomial better than 1, 2, 3, or 4? Let's find out:
fit.1 = lm(wage~age, data=Wage)
fit.2 = lm(wage~poly(age,2), data=Wage)
fit.3 = lm(wage~poly(age,3), data=Wage)
fit.4 = lm(wage~poly(age,4), data=Wage)
fit.5 = lm(wage~poly(age,5), data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5) 
# anova tests the null that M1 is sufficient by running  F-tests comparing the RSS to those of the complex models
# The tests are sequential, comparing model 1 to model 2, then model 2 to model 3, then 3 to 4, finally 4 to 5.
# We find that 2 beats 1, 3 beats 2, but 4 does not beat 3 and 5 does not beat 4. So 3 wins.

# Note: in this particular case, the orthogonality of the polynomials does this for us:
summary(fit.5)
# the p-values on the coefficients are identical to the anova results. Why? Because
# when the variables are orthogonal, the covariance matrix is diagonal and hence only
# their variances matter. In this case, the t-tests capture all of the information
# needed for the hypothesis tests. In fact, the square of the t-stats are the F-stats.
(-11.983)^2

# When there are other variables, we should use F-stats though:
fit.1 = lm(wage~education+age, data=Wage)
fit.2 = lm(wage~education+poly(age,2), data=Wage)
fit.3 = lm(wage~education+poly(age,3), data=Wage)
anova(fit.1,fit.2,fit.3)

fit = glm(I(wage>250)~poly(age,4), data=Wage, family=binomial)
summary(fit)

preds = predict(fit, newdata=list(age=age.grid), se=T)
str(preds)
range(preds$fit)
# note: the fitted values are outside of (0,1). Why? Because by default
# it returns the type="link" function, which is X*beta. But we want 
# Pr(Y=1|X) = exp(XB)/(1+exp(XB))
# We could do this manually:
pfit.1 = exp(preds$fit)/(1+exp(preds$fit))
# Or by using the type="response" argument:
pfit.2 = predict(fit, newdata=list(age=age.grid), se=T, type="response")
max(pfit.1-pfit.2$fit)
# problem: the standard errors in the fitted probabilities imply error bars outside (0,1)
# so we use the error bars on XB to find error bars on Pr explicitly:

pfit = pfit.1
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

mean(wage>250) # about 2% of people have wage >250k. so y axis range shouldn't be too big
plot(age,I(wage>250), xlim=agelims, type="n", ylim=c(0,0.2))
points(jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey") 
#jitter() adds noise to a vector so when they're plotted the points don't stack on top of each other (or to break ties randomly)
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue")

table(cut(age,4)) 
# cut picks 4 cutpoints of its input and bins them, returning an equal length vector of bins (as factors)
# since they're factors, we can use them on the RHS of a linear regression:
fit = lm(wage~cut(age,4), data=Wage)
# this created a piecewise constant function (or "step" function)
coef(summary(fit))

#### Splines
par(mfrow=c(1,1))
library(splines)
# Recall that splines can be fit using OLS on an appropriate set of basis functions
# We can get these with bs(), for B-spline. Then just plug them into OLS
fit = lm(wage~bs(age,knots=c(25,40,60)), data=Wage) # by default does cubic spline
summary(fit)
head(bs(age,knots=c(25,40,60))) # should have order+knots = 3+3 = 6 columns
# note: could have also just specified the degrees of freedom:
dim(bs(age,df=6))
attr(bs(age,df=6),"knots") # it split is at equal percentiles (3 knots: 25%/50%/75%)

pred = predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty="dashed") # line type
lines(age.grid, pred$fit-2*pred$se, lty="dashed") 

# Natural Spline (with linear boundaries) using ns(), for a natural spline basis
fit2 = lm(wage~ns(age, df=4), data=Wage)
pred2 = predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2) # note how the right boundary is more naturally straight

head(ns(age,df=4)) # df will equal size of the matrix. 
attr(ns(age,df=4), "knots")

# alternatively, just define the knots manually, either by picking them with, e.g., knots=c(35, 50, 125), or by choosing quantiles
head( ns(age, knots=quantile(age,probs=c(0.25,0.5,0.75) ) ) )
attr( ns(age, knots=quantile(age,probs=c(0.25,0.5,0.75) ) ), "knots" )


# Smooting Spline with smooth.spline()
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age, wage, cv=TRUE) # cross validate to find optimal effective df
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=.8)

# Local Regression with loess()
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Local Quadratic Regression")
fit  = loess(wage~age, span=.2, data=Wage)
fit2 = loess(wage~age, span=.5, data=Wage)
lines(age.grid, predict(fit, newdata=data.frame(age=age.grid)), col="red", lwd=2) # note: loess, the newdata= input must be a dataframe (can't be a list)
lines(age.grid, predict(fit2, newdata=data.frame(age=age.grid)), col="blue", lwd=2) 
legend("topright",legend =c("Span =0.2" ," Span =0.5"), # note: we can break commands on multiple lines
       col=c("red "," blue "),lty =1, lwd =2, cex =.8)

# as span -> infty, the model converges to pooled OLS
fit3 = loess(wage~age, span=100, data=Wage, degree=1)
fit4 = lm(wage~age, data=Wage)
fit5 = loess(wage~age, span=100, data=Wage, degree=2)
fit6 = lm(wage~poly(age,2), data=Wage)
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
lines(age.grid, predict(fit3, newdata=data.frame(age=age.grid)), col="blue", lwd=2) 
lines(age.grid, predict(fit4, newdata=data.frame(age=age.grid)), col="red", lwd=2) 
lines(age.grid, predict(fit5, newdata=data.frame(age=age.grid)), col="blue", lwd=2, lty="dashed") 
lines(age.grid, predict(fit6, newdata=data.frame(age=age.grid)), col="red", lwd=2, lty="dashed") 
# note: the locfit library can also do these computations

### GAMs: wage = constant+f(year)+f(age)+education. Note that education is a dummy variable, so it's already non-parametric.
# If we want to do a natural spline, we can do this simply using the lm() function
gam1 = lm(wage~ns(year,4)+ns(age,5)+education, data=Wage)

# But for more complicated things like smoothing splines, we need to use the gam library
#install.packages("gam")
library(gam)
# the s() function is for smoothing splines
gam.m3 = gam(wage~s(year,4)+s(age,5)+education, data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue") 
# plot() realizes that gam.m3 is a gam object. so it appleis the plot.gam() plotting methods
# but we can apply these methods to non-gam objects, like gam1, which is an lm() object.
plot.gam(gam1, se=T, col="red")
# had we used the generic plot, it wouldn't have had the same functionality:
#plot(gam1, se=T, col="red") # it would have returned the regression diagnostics

# Look at the graph for "year". It looks rather linear, so maybe the 4th order 
# polynomial is overfitting it. Let's test this with ANOVA, focusing on orders 0, 1, and 5
gam.m1 = gam(wage~s(age,5)+education, data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F") # looks like model 2 (the linear one) wins
summary(gam.m3)
# the last part of this summary tests the null of a linear relationship versus the alternative of a non-linear one
# it does not reject linearity in year, but does reject it in age.
preds = predict(gam.m2, newdata=Wage)
# gams can also do local regressions using lo(). Let's do a local regression on age and a spline on year
gam.lo = gam(wage~s(year,df=4)+lo(age,span=.7)+education, data=Wage)
plot.gam(gam.lo, se=T, col="green")

# for 2-dimension local regressions, we can put multiple variables into lo(), and then plot them in 3D using the akima library
gam.lo.i = gam(wage~lo(year,age,span=0.5)+education, data=Wage)
#install.packages("akima")
library(akima)
par(mfrow=c(1,2))
plot(gam.lo.i)

# Logistic GAM
gam.lr = gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green") # notice the problem: no high earners in the <HS category creates perfect separation. so we need to drop these
table(education, I(wage>250))

gam.lr.s = gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage, subset=(education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green") 
