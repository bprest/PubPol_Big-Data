# ISLR Chapter 9 Lab

#install.packages("e1071")
library(e1071)
# This is the SVM library
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1 # For class 1, shift x values up by 1. This separates the classes


plot(x, col=(3-y)) # endogenously determines color based on y values. for y=1, col=2 (red). for y= -1, col=4 (blue)
# Not linearly separable

#svm() does SVM regression or classification.
# If you want it to do classification, you need to ensure that the y variable is a factor
df = data.frame(x=x, y=as.factor(y))

svmfit = svm(y~., data=df, kernel="linear", cost=10, scale=FALSE)
# The linear kernel means don't use a kernel (i.e., do a support vector classifier, not machine)
# cost is the margin violation tuning parameter. This is the PENALTY term for violations (i.e., not the budget term from the text).
#   High cost= means a strong
# scale tells svm() not to standardize variables to have mean=0 and sd=1. In some cases, we might want to do this.

plot(svmfit, df)
# The points on the graph show the types by color (black= -1, red = +1)
# The jagged boundary is an artifact of the plotting function. It's actually linear.
# The points labelled "x" are support vectors. The points labelled "o" are not.
# Only one observation is misclassified: the red "x" at the bottom.
plot(svmfit, df, ylim =c(-2,3), xlim=c(-3,3))

names(svmfit)
svmfit$index # this shows the identities of the support vectors.
data.frame(x[svmfit$index,],y[svmfit$index])

summary(svmfit)

# Smaller cost penalty parameter should mean we are more tolerant of errors, allowing the margin to be wider (and hence more SVs).
svmfit = svm(y~., data=df, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, df) # plot shifts to the right. more SVs (x's)

# Choose cost parameter through CV. 
# e1071 has built-in tune() function that will do this for you. 
# But it can also tune other methods. See ?tune.wrapper for options.
# Let's tune a linear kernel SVM (aka an SVC), over cost parameters of (0.001, 0.01, 0.1, 1, 5, 10, 100)
set.seed(1)
tune.out = tune(svm, y~., data=df, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# The best cost parameter turns out to be 0.1.
names(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
plot(bestmod, df)

# Let's predict out of sample.
xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdf = data.frame(x=xtest, y=as.factor(ytest))

ypred = predict(bestmod, testdf)
table(predict=ypred, truth=testdf$y)
# Pretty good: 19/20 observations correctly classified

# What if we used a lower cost penalty?
svmfit = svm(y~., data=df, kernel='linear', cost =0.01, scale=FALSE)
ypred = predict(svmfit, testdf)
table(predict=ypred, truth=testdf$y)
# Worse: 18/20 correct.

# Let's see what happens when the classes are actually linearly separable
x[y==1 ,]= x[y==1 ,]+0.5 # separate them more
plot(x, col=(y+5)/2, pch=19)

# Try with a high cost parameter (so we don't misclassify at all, but rely on only a few SVs and so have high variance)
df=data.frame(x=x,y=as.factor (y))
svmfit =svm(y~., data=df , kernel ="linear", cost =1e5)
summary (svmfit)
plot(svmfit, df)
# Note the narrow margins

# Try with a smaller cost parameter (so we might misclassify, but have lower variance since we're relying on more SVs)
svmfit =svm(y~., data=df , kernel ="linear", cost =1)
summary(svmfit)
plot(svmfit ,df)
# This perhaps looks better from a variance point of view, but not too much worse from a bias point of view.


## SVM: Now use non-linear kernels
set.seed(1)
x=matrix (rnorm (200*2) , ncol =2)
x[1:100 ,]=x[1:100 ,]+2
x[101:150 ,]= x[101:150 ,] -2
y = c(rep(1,150), rep(2,50))

dat=data.frame(x=x,y=as.factor (y))

# 200 observations. First 100 are low "1s", next 50 are high "1s", and last 50 are middling 0s.
plot(x, col=y) # Note that no linear classifier will work well here.

train = sample(200,100)
# Let's try a radial kernel
svmfit = svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1) 
# Alternatively: use polynomial of degree 2 (CV suggests degree=2, cost=0.1, gamma=1)
svmfit = svm(y~., data=dat[train,], kernel="polynomial", degree=2, cost=0.1, gamma=1)
#tune.out = tune(svm, y~., data=dat[train,], kernel="polynomial", ranges=list(degree=2, cost=c(0.1,1,5,10), gamma=c(0.5,1,2)))
#plot(tune.out$best.model, dat[train,])
# gamma is the kernel parameter that stresses the importance of distance (high gamma => large shrinkage of distance)
plot(svmfit, dat[train,])
summary(svmfit)

# Note that there are a lot of training errors (black points in the purple, 
# and red points in the blue). We can reduce these by increasing the cost 
# penalty, but this gives an irregularly-shaped boundary.
svmfit = svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5) 
plot(svmfit, dat[train,])
# Now almost all the red correctly appear in the purple, and almost all the black appear in the blue.

# Let's pick cost and gamma by CV, using tune()
set.seed(1)
tune.out = tune(svm, y~., data=dat[train,], kernel="radial", 
                ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
plot(tune.out$best.model, dat[train,])

# Error table
table(true = dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train,])) 
(21+18)/(21+18+56+5) # = 39% error rate.

### ROC Curves (receiver operating characteristics)
#install.packages("ROCR")
library(ROCR)
# This function takes the truth and prediction and outputs a ROC graph.
# We do this because plot.performance() must take a performance class object, 
# which in turn is created using a prediction class object. And we don't want to
# redo these steps every time we make a new graph.

# Basically what this does is try to classify using a numeric score (pred) with
# various degrees of leniency over false positives, and reports the true positive 
# rate as a function of the false positive rate. Then it graphs it.
rocplot = function(pred, truth, ...) {
        predob = prediction(pred, truth)
        perf = performance(predob, "tpr", "fpr") # tpr = true positive rate, fpr = false positive rate
        plot(perf, ...)
}

# We're going to need the fitted values for each observation. 
# svm() and predict.svm() can output this if you ask using decision.values=TRUE

svmfit.opt = svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=TRUE)
fitted = attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"], main="Training Data")

# If we increase gamma, we reduce the emphasis on large distances, producing a 
# fit that's more locally flexible, and hence has better training performance.
svmfit.flex = svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values=TRUE))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

# But on the test data, the less flexible gamma=2 seems to work better.
fitted = attributes(predict (svmfit.opt ,dat[-train,], decision.values =T))$decision.values
rocplot(fitted ,dat [-train ,"y"], main ="Test Data")
fitted = attributes(predict (svmfit.flex ,dat[-train,], decision.values =T))$decision.values
rocplot(fitted ,dat [-train ,"y"], add=T,col ="red")

### SVM with Multiple Classes: Using 1-vs-1 approach.
set.seed(1)

x = rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(rep (1 ,150) ,rep (2 ,50) )
y = c(y, rep(0,50)) # add a third class, class 0.
x[y==0,2] = x[y==0,2] + 2
dat = data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x[,2],x[,1], col=(y+1))

svmfit = svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)
#plot(svmfit, dat, ylim=1.05*range(x[,1]), xlim=1.05*range(x[,2]))

# SVM can also do regression if you pass it continuous dependent variables
y = 2+5*x[,1]
dat = data.frame(x=x[,1],y=y)
svmfit = svm(y~., data=dat, kernel="linear", cost=10, gamma=1)
plot(svmfit, dat)
# Can't plot SVM regression unfortunately


### Application to Gene Expression Data
library(ISLR)
search()

names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# Large number of variables (2308), low N (20 in test, 63 in train)
table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out = svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
names(out)

table(out$fitted, dat$y)
# No training errors (of course, since p>n, a linear model would find Rsq=1, hence training MSE=0)
lm.fit = lm(y~., data=cbind(dat[,1:ncol(dat)-1],y=as.numeric(dat[,ncol(dat)])))
summary(lm.fit)
sum((lm.fit$residuals)^2)

# But on the test set...

dat.test = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.test = predict(out, newdata=dat.test)
table(pred.test, dat.test$y)
# A few errors: 2 out of 20 with cost=10

#tune.out = tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
#summary(tune.out)
