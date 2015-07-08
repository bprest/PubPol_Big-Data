### ISLR Chapter 8 Lab

## Numerical proof showing that about 63% of observations should appear in each
# bootstrapped sample. It's limit of (1-1/n)^n = 1/e = 0.368
# Recall the limit of (1+r/n)^n = e^r. Here, r = -1, so it converges to 1/e.
n = 100
xgrid = seq(1,n)

p = (1-1/xgrid)^xgrid
plot(xgrid,p)

(1-1/1000000)^1000000
1/exp(1)


## Fitting 
#install.packages("tree")
rm(list=ls())
#search()
#detach(Carseats)

library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8,"No","Yes")

Carseats = data.frame(Carseats,High)

# Fit a classification tree in order to predict whether a seat has High sales
tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)
# Note: RME is -2*sum(n_mk*log(p_mk)) where the sum is over m and k.
# It is divided by n-|T_0| = 400 - 27 = 373
plot(tree.carseats)
text(tree.carseats, pretty=0) 
# if pretty is on, it abbreviates the qualitative variables (e.g. "M" instead of "Medium")
tree.carseats
# Breaks out branches and overall prediction within each (including %ages)
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High~.-Sales, Carseats[train,])
summary(tree.carseats)
tree.pred = predict(object = tree.carseats, newdata = Carseats.test, type="class")
T = table(tree.pred, High.test) # something random here. probably breaking ties.
# Error rate
(T[2,1]+T[1,2])/sum(T)

# Now let's prune the tree. The level of pruning is determined using CV.
# The function that determines the pruning is set using the FUN= argument. 
# Use error rate, which is given by the prune.misclass() function
prune.misclass(tree.carseats) # On its own, it does the training data, which
# obviously finds that a larger tree fits better.
summary(tree.carseats) 
# k is the tree complexity tuning parameter
summary(prune.misclass(tree.carseats, k= -0.000001)) # a negative tuning parameter implies no penalty
summary(prune.misclass(tree.carseats, k=0)) # a weakly positive parameter starts penalizing
summary(prune.misclass(tree.carseats, k=10)) # the larger the tuning parameter, the more pruning
# Note: prune.misclass() is shorthand for prune.tree(..., method="misclass")

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
# dev corresponds to the CV test MSE (it's called dev for programming reasons)
# Best test MSE is with 9 terminal nodes.
optsize = cv.carseats$size[which.min(cv.carseats$dev)]

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")
# This plot shows that the test MSE is best for a 9-node tree, which corresponds to a tuning parameter of 1.75.

prune.carseats = prune.misclass(tree.carseats, best=optsize) 
# returns the best tree of the "best=" size, where best is determined by the misclassification method
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(94+60)/200 # 77% success rate. better than before (71.5% rate), plus the tree is simpler.
# Note that if we increased the tree size from 9 to 15, our test success rate will be worse:
prune.carseats = prune.misclass(tree.carseats, best=15) 
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86+62)/200


### Fitting Regression Trees
rm(list=ls())
dev.off()
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b') # recall type="l" is lines, type="p" for points, type="b" for both
# best tree size is 8, the most complex one
# if we wanted to prune:
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# But CV says 8, so we use 8
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test) # predicted values versus actual
abline(0,1) # 45-degree line (0 intercept, slope 1) for comparison.
mean((yhat-boston.test)^2)
# Test MSE around 25.

### Bagging & Random Forests
#install.packages("randomForest")
# Note: the randomForest() function can do both RF and bagging. Recall that 
# random forest = bagging when m=p. So to do bagging we set m=p with "mtry="
library(randomForest)
set.seed(1)

## Bagging
# For bagging, m should equal the number of variables in Boston minus the dependant variable
dim(Boston)
m = dim(Boston)[2] - 1
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry = m, importance=TRUE)
bag.boston
# Note: by default it did B=500 (500 trees), 
# importance determines whether the function outputs statistics showing the relative importance of predictors

# How does it do in the test set? Predict them for test, compare to actual test obs:
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)
# Note that this test MSE of 13 is much better than the 25 found under the optimally pruned single tree. 

# Note: if you want to do more or fewer trees, you can set that with ntree
randomForest(medv~., data=Boston, subset=train, mtry = m, importance=TRUE, ntree=25)

## Random Forest
set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=T)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
# This test MSE is even better than bagging 11, since it has lower variance through lower correlation of the bootstrapped estimates.
importance(rf.boston)
# The first measure of importance is based on the % change in MSE (based on out-of-bag samples) when the variable is removed.
# The second average measure is the change in node impurity due to that variable's split. This is based on the training RSS.
# The most important variables here are house size and community wealth.
varImpPlot(rf.boston)


### Boosting
# Recall boosting is like bagging, but each iteration learns from the previous ones.
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4) # depth is size of each tree
# Note: the default shrinkage parameter is set to 0.001.
# For a classification problem, we would use distribution="bernoulli" (0/1).
summary(boost.boston)
# Partial Dependence Plots
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")
# These plots produce f(x) of the chosen variable (x), after integrating out the other variables.
# It reveals that df/d(rm)>0, and convex, while df/d(lstat)<0 and convex.

yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=length(boost.boston$trees))
mean((yhat.boost-boston.test)^2)

# Changing the shrinkage parameter:
boost.boston = gbm(medv~., data=Boston[train,], distribution="gaussian",
                   n.trees=5000, interaction.depth=4, shrinkage=0.2)

yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=length(boost.boston$trees))
mean((yhat.boost-boston.test)^2)
# It turns out the a larger shrinkage parameter (faster learning) gives a better
# test MSE (11.5 vs. 11.8), holding n.trees constant

# Increasing n.trees (more time to learn):
boost.boston = gbm(medv~., data=Boston[train,], distribution="gaussian",
                   n.trees=10000, interaction.depth=4, shrinkage=0.2)

yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=length(boost.boston$trees))
mean((yhat.boost-boston.test)^2)
# Even better: 10.9.

# Increasing n.trees (more time to learn):
boost.boston = gbm(medv~., data=Boston[train,], distribution="gaussian",
                   n.trees=20000, interaction.depth=4, shrinkage=0.2)

yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=length(boost.boston$trees))
mean((yhat.boost-boston.test)^2)
# Worse: 11.4. Eventually we "learn" too much.