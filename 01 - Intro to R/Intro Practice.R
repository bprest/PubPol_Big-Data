# This is a practice file, showing the basics of R.
# Brian Prest
# 5/26/2015

#### Part 1: Using R Interactively (thru the Console)
# In Console:
1+2
x=7
y=2*x

#### Part 2: Using Scripts in RStudio
# But we want to save this stuff. So we write it all down in a script (like a do file, for those of you who have done Stata)
# Once it is written down, we can execute the lines directly from the script, either in chunks (line by line) or all at once.

# RStudio Commands: 
# Run selected lines:                                 Ctrl+Enter
# Run everything from the [B]eginning to the cursor:  Ctrl+Alt+B
# Run everything from the the cursor to the [E]nd:    Ctrl+Alt+E
# Run everything:                                     Ctrl+Alt+R

# Part 1: Defining variables
x = 7
x = c(1,3,2,4) # Define a vector (a multidimensional variable; here, 1x4)
x <- c(1,3,2,4) # Same thing
y = c(1,2,3,4,5)
length(x)
length(y)

#x+y # Doesn't work! They are different sizes!
y = c(7,8,9,10)
z = x+y
# Now it works

# List function: Lists all objects currently held in R
ls()

# Remove function: delete objects
rm(z)
?rm 
# Help function. It looks like the remove function has two ways to enter variables for deletion: 
# either list them with commas separating them, or create a character vector listing the ones to delete.
delvars = ls()
rm(list=delvars)
rm(delvars)
rm(list=ls()) # Chaining commands can be shorter
ls()

# Matrices
?matrix # It takes arguments: data=, nrow=, ncol=, byrow=, and dimnames=

X = matrix(data=c(1,2,3,4,5,6), nrow=3, ncol=2)
Y = matrix(ncol=2, nrow=3, data=c(1,2,3,4,5,6)) # Same thing--order doesn't matter so long as you specify the argument
X
Y
dim(X) # Find the size/dimension of the matrix
# But order does matter if you ignore the arguments
X = matrix(c(1,2,3,4,5,6), 3, 2) # here, it automatically interprets the c() as the "data" argument because it's first, 3 as the nrows because it's second, etc.)
Y = matrix(2, 3, c(1,2,3,4,5,6)) # here, it thinks that the data is just "2", and repeats it for 3 rows. It interprets 1 column by just taking the first value in the c() vector, which is 1
dim(X) # returns the dimensions of the matrix (2 rows, 3 cols)

# Algebra on vectors is pointwise by default
x = c(1,2,3,4)
2*x
x*x 
sqrt(x)
x^2
x + 2

# To do matrix multiplication, we need to use the % % modifier:
X = matrix(c(1,2,3,4,5,6),2,3) # 2x3
Y = matrix(c(6,5,4,1,2,3),3,2) # 3x2
#X*Y # (2x3)*(3x2) doesn't make sense pointwise, but does with matrix multiplication
X%*%Y # = XY, matrix multiplication
t(X) # transpose matrix
# For more matrix operations, see http://www.statmethods.net/advstats/matrix.html

# Merging and appending matrices:
# "Merge" = Column Bind. E.g., Same observations, different variables. Must have same number of rows
Z=t(Y)
X
Z
cbind(X,Z)

# "Append" = Row Bind. E.g., adding new observations with the same variables.
rbind(X,Z)

# Take means/sums by row/col
rowSums(X)
rowMeans(X)
colSums(X)
colMeans(X)

# Random Number Generators (RNGs), and How Computers Generate Random Numbers
set.seed(1303) # 1303 is arbitrary
rnorm(5,mean=0,sd=1)

### Graphics

x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x,y, xlab = "Our X-axis label", ylab = "Our Y-axis label", main = "Our Graph Title")

mypath = file.path("C:","Users","Brianprest","OneDrive","Grad School","TAing","Big Data","Git") 
pdflocation = mypath
#pdf(paste(mypath,"/Gobbledegook.pdf", sep="")) # Not working
dev.off() # We're done developing the grpah.

### Contour plots. Level sets (like indifference curves) or topological maps
x = 1:10
#x = seq(1,10) # same thing
x = seq(-pi,pi,length=50) # 

# We're going to make a contour plot, which has 3 dimensions. Let's make a plot with a square ranging from (x,y)=(-pi,-pi) to (pi,pi), and plot cos(y)/(1+x^2) in this range
y = x
f = outer(x,y,function(x,y) cos(y)/(1+x^2))
# The logic is this: for each of the 50x50 combinations of x and y values, evaluate the function of (x,y) defined as f=cos(y)/(1+x^2). Return this as a 50x50 matrix.
contour(x,y,f)
contour(x,y,f, nlevels=10) # same thing
contour(x,y,f,nlevels=45) # plots with more level sets

fa = (f-t(f))/2
contour(x,y,fa,nlevels=15)

# image(): Similar, but with heatmap style graph
image(x,y,fa)

# persp(): 3-D Perspective graph
persp(x,y,fa)
persp(x,y,fa,theta=30) # theta controls the horizontal rotation (30 degrees clockwise from default)
persp(x,y,fa,theta=30, phi=25) # phi controls the vertical rotation (25 degrees above default)

### Indexing
# Vectors
x = c(4,9,7,2)
x[1:3] 
1:3
x[c(1,2,3)] # same thing
x[c(3,2,1)] # reversed

# Matrices
A = matrix(16:1,4,4)
A
A[2,3] # matrix A, [row,column]
# Suppose we want to grab a certain set of rows and columns. 
# Just the last 2 rows, middle 2 columns
A[3:4,2:3]
# Say we want rows 1 and 4, columns 2 and 3. Then we can do
A[c(1,4),c(2,3)]
# Leave blank = get all
A[2:4,] 
# Non-indexing
A[-2,] # all rows except 2
A[-(2:3),] # all rows except 2 and 3


## THIS NEEDS WORK
### Working with Strings
paste("apple","banana","candy")
paste("apple","banana","candy",sep="--")
paste("apple","banana","candy",sep="")

wdir = 'C:\\Users\\Brianprest\\OneDrive\\Grad School\\TAing\\Big Data\\Git'
wdir
list = dir(wdir)
list[2]



### Reading in Data Files
Auto = read.table("C:/Users/bcp17/OneDrive/Grad School/TAing/Big Data/ISLR/Data/Auto.data", nrows=10) # Reading just the first 10 rows to get a "peek" at the file
Auto
# This reveals that the first row is variable names, but R interpreted them as data with names v1, v2, v3...
Auto = read.table("C:/Users/bcp17/OneDrive/Grad School/TAing/Big Data/ISLR/Data/Auto.data", header=T)
Auto
typeof(Auto)
#fix(Auto)

# Note that some rows have missing values, which are represented by the "?" character. Let's make R parse "?" as a missing value.
Auto = read.table("C:/Users/bcp17/OneDrive/Grad School/TAing/Big Data/ISLR/Data/Auto.data", header=T, na.strings="?")
Auto
typeof(Auto)
#fix(Auto)

dim(Auto)
Auto[1:5,]

# Drop the observations with ANY missing variables. 
Auto = na.omit(Auto)
dim(Auto) # dropped 5 obs
names(Auto) # variable names


# Let's plot cylinders versus mpg (thought: V8 engines less efficient than 4 cylinder engines)
#plot(cylinders, mpg) # DOesn't work, since "acceleration" are not objects--Auto is the object.
# We can refer to variables within an object by using the $ character:
plot(Auto$cylinders, Auto$mpg)
# Or we can explicitly tell R to interpret use variable names within the Auto object
attach(Auto)
plot(cylinders, mpg)

# Factor variables: Replace the cylinders variables with a categorical variable (as opposed to a numerical variable)
c(cylinders[1],2*cylinders[1])
cylinders = as.factor(cylinders)
#c(cylinders[1],2*cylinders[1]) # Now it doesn't work
# When making graphs, R will determine the most appropriate plot type depending on how the variable is set.
plot(cylinders, mpg) # Boxplots, since cylinders are categorical
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T, xlab="mpg", ylab="cylinders")
tabulate(cylinders)
tabulate(mpg)

# Histograms
hist(mpg)
hist(mpg,col=2) # col=2 and col="red" do the same thing
hist(mpg, col=2, breaks=15) # col=2 and col="red" do the same thing

# Scatterplot Matrix
pairs(Auto)
pairs(~mpg + displacement + horsepower + weight + acceleration,Auto)

plot(horsepower, mpg)
# can identify outlier points with the identify() function.
#identify(horsepower,mpg,name) # need to click finish to end the identify function

summary(Auto)
summary(mpg)


# Calling whole functions with source('filepath')


