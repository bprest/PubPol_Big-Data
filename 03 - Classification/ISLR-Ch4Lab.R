## Brian Prest
## 6/9/2015
## Intro to Statistical Learning: Chapter 4 Lab

#### Stock Market Data
library(ISLR)
head(Smarket)
names(Smarket)
summary(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)
days = 1:nrow(Smarket)
abline(lm(Volume~days, data=Smarket))

# Logistic Regression

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial) # Note: first level is zero, second level is 1. To check this:
contrasts(Direction)
summary(glm.fit)
summary(glm.fit)$coef
coef(glm.fit)

predict(glm.fit) # by default, returns the "link" function, which here means the log-odds ratio, = X*beta
exp(predict(glm.fit)[1])/(1+exp(predict(glm.fit)[1])) 
glm.probs = predict(glm.fit, type = "response") # "response" says return P(Y=1|X), or exp(X*beta)/(1+exp(X*beta))

# Also by default uses the training data that was used for the regression. Alternatively, we couuld provide it newdata= to predict on. To see these options, type ?predict.glm
glm.pred = rep("",nrow(Smarket))
glm.pred[glm.probs>0.5] = "Up"
glm.pred[glm.probs<=0.5] = "Down"

table(glm.pred)
table(Direction)
table(glm.pred, Direction)
mean(glm.pred==Direction) # 52% correct. This is terrible. We can always get at least 50% right by setting all parameters = 0.

as.numeric(TRUE)
as.numeric(FALSE)


# Now do cross-validation. Use 2001-2004 as training set, and 2005 as test set
train = (Year<2005)
Smarket.2005 = Smarket[!train,] # test set X
Direction.2005 = Direction[!train] # test set y

glm.fit2 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train) # Note: first level is zero, second level is 1. To check this:
summary(glm.fit2)
rm(glm.probs)
glm.probs = predict(glm.fit2, Smarket.2005, type = "response")
#glm.probs = predict(glm.fit2, newdata = Smarket.2005, type = "response", se.fit=TRUE) # if you need standard errors

glm.pred = rep("",nrow(Smarket.2005))
glm.pred[glm.probs>0.5] = "Up"
glm.pred[glm.probs<=0.5] = "Down"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # worse than random guessing!

# Lets toss out the very insignificant variables. They may just be adding noise
glm.fit = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset = train)

glm.probs = predict(glm.fit, Smarket.2005, type="response")
glm.pred = rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5] = "Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # worse than random guessing!

# Say that we know that yesterday's change was +1.2% and the day before that was 1.1%. What is the probability that the market is going to rise today?
predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1, -0.8)), type="response") # Less than 50%


#### Linear Discriminant Analysis
# lda is part of the MASS library
?lda
library(MASS)
?lda

lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
mean(Smarket$Direction[train]=="Up")
lda.fit # First part: just the share of each category in the data. Second Part: simple averages of input data. Third Part:
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005) 
# Predict produces a list with 3 elements
# 1) Class: the LDA's predicted class (based on P>=50%)
# 2) posterior: the LDA's predicted outcome. An NxK matrix (based on the LDA, P(Yi=k|Xi))
# 3) The value of the linear discriminant (if it's large, it predicts the market will go up)

table(lda.pred$class, Direction.2005)
mean(lda.pred$class==Direction.2005)

#### QDA:
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005) # about 60% accuracy. Pretty good actually!


### KNN
library(class) # classification tools package
train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]
set.seed(1) # knn() uses RNG to break ties when two observations are equally far apart
knn.pred = knn(train = train.X, test = test.X, cl = train.Direction, k = 1) # training data, test data, class data, choice of k
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005) # exactly 50% of the time

knn.pred = knn(train = train.X, test = test.X, cl = train.Direction, k = 3) # training data, test data, class data, choice of k
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005) # 53.6% of the time. But QDA gave 60%, which is better.

### Caravan Insurance
dim(Caravan)
attach(Caravan)
summary(Purchase)

# up = (Direction=="Up")
# plot(Lag1[up], Lag2[up], pch="x", col="darkgreen")
# points(Lag1[!up], Lag2[!up], pch="o",  col="red")

standardized.X = scale(Caravan[,-86]) # standardizes variables to mean 0 variance 1.
apply(Caravan[,-86],2,var) # apply(X, MARGIN, FUN) where margin=1 means rows and =2 means columns
apply(standardized.X,2,var) # apply(X, MARGIN, FUN) where margin=1 means rows and =2 means columns

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train = train.X, test = test.X, cl = train.Y, k=1)
table(knn.pred,test.Y)

mean(knn.pred!=test.Y) # 12% error rate looks good, but it's not. We can get 6% failure by always predicting "No" (since very few people buy insurance anyway):
mean(test.Y!="No")

# What if the question is, "what fraction of all individuals are correctly predicted to buy insurance?". That is, what is our true positive rate?
# 68+9=77 were predicted yes, and 9 of those were correct. That's 12%, better than random guessing:
tab = table(knn.pred,test.Y)
tab[2,2]/sum(tab[2,])
9/(68+9) # 12% true pos

knn.pred = knn(train = train.X, test = test.X, cl = train.Y, k=3)
table(knn.pred,test.Y)

mean(knn.pred!=test.Y) # Now down to 7.5% error rate
mean(test.Y!="No")

tab = table(knn.pred,test.Y)
tab[2,2]/sum(tab[2,]) # 19% true pos

knn.pred = knn(train = train.X, test = test.X, cl = train.Y, k=5)
table(knn.pred,test.Y)

mean(knn.pred!=test.Y) # Now down to 5.9% error rate
mean(test.Y!="No")

tab = table(knn.pred,test.Y)
tab[2,2]/sum(tab[2,]) # 27% true pos

# Compare to logit
glm.fit = glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred,test.Y) # No Yeses correctly predicted with a 50% cutoff

glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.25] = "Yes"
table(glm.pred,test.Y) # 33% predicted Yes's correct with a 25% cutoff
11/33




