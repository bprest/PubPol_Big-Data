# This is a practice file, showing the basics of R.
# Brian Prest
# 5/31/2015
rm(list=ls())
root = "bcp17"
#root = "Brianprest"


## Q8

#gitdir = file.path("C:","Users",root,"OneDrive","Grad School","TAing","Big Data","Git") 
datadir = file.path("C:","Users",root,"OneDrive","Grad School","TAing","Big Data","ISLR","Data") 

college = read.csv(paste(datadir,"/College.csv", sep=""))

rownames(college)=college[,1]
college = college[,-1]
fix(college)

summary(college)
pairs(college[,1:10])

attach(college)
 
plot(Private,Outstate, xlab="Private", ylab="Tuition")

college[Accept>=26300,]

Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

summary(college$Elite)

plot(Elite,Outstate, xlab="Elite", ylab="Tuition")

par(mfrow=c(2,2))
hist(Outstate, breaks=10)
hist(PhD, breaks=15)
hist(Grad.Rate, breaks=20)
hist(Accept, breaks=25)

plot.new()
dev.off()

### Q9

Auto = read.table(paste(datadir,"/Auto.data",sep=""), header=T, na.strings="?")
attach(Auto)

range(weight)
mean(weight)
sd(weight)

auto_trim = Auto[-10:-80,]
range(auto_trim$weight)
mean(auto_trim$weight)
sd(auto_trim$weight)

plot(Auto)


### Q10

library(MASS)
attach(Boston)
plot(Boston)
par(mfrow=c(1,3))
hist(crim, breaks=10)
hist(tax, breaks=15)
hist(ptratio, breaks=20)
Boston[tax>600,]
Boston[ptratio<14,]
summary(chas)
sum(chas)
median(ptratio)
range(medv)
Boston[medv==5,]
summary(rm>=7)
summary(rm>=8)
Boston[rm>=8,]

lm.fit = lm(medv~crim+nox+chas+indus+zn+lstat+tax+rm+age)
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)
