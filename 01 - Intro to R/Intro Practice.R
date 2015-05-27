# This is a practice file, showing the basics of R.
# Brian Prest
# 5/26/2015

wdir = 'C:\\Users\\Brianprest\\OneDrive\\Grad School\\TAing\\Big Data\\Git'
mypath = file.path("C:","Users","Brianprest","OneDrive","Grad School","TAing","Big Data","Git") 

#### Part 1: Using R Interactively (thru the Console)
# In Console:
1+2
x=7
y=2*x

#### Part 2: Using Scripts in RStudio
# But we want to save this stuff. So we write it all down in a script (like a do file, for those of you who have done Stata)
# Once it is written down, we can execute the lines directly from the script, either in chunks (line by line) or all at once.

# RStudio Commands: 
# Run selected lines:                              Ctrl+Enter
# Run everything from the Beginning to the cursor: Ctrl+Alt+B
# Run everything from the the cursor to the End:   Ctrl+Alt+E
# Run everything:                                  Ctrl+Alt+R

# Part 1: Defining variables
x = 7
x = c(1,3,2,4) # Define a vector (a multidimensional variable; here, 1x4)
x <- c(1,3,2,4) # Same thing
y = c(1,2,3,4,5)
length(x)
length(y)

#x+y # Doesn't work! They are different sizes!
# Indexing
x[1:3] 
1:3
x[c(3,2,1)]

z = x + y[1:4] # Comformable vectors

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

# Algebra on vectors is pointwise by default
x = c(1,2,3,4)
2*x
x*x 
sqrt(x)
x^2
x + 2


# To do matrix multiplication, we need to use the % % modifier:
X = matrix(c(1,2,3,4),2,3) # 2x3
Y = matrix(c(4,1,2,3),3,2) # 3x2
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

# persp(): 3D Perspective graph
persp(x,y,fa)
persp(x,y,fa,theta=30) # theta controls the horizontal rotation (30 degrees clockwise from default)
persp(x,y,fa,theta=30, phi=25) # phi controls the vertical rotation (25 degrees above default)


### Working with Strings
paste("apple","banana","candy")
paste("apple","banana","candy",sep="--")
paste("apple","banana","candy",sep="")

wdir
list = dir(wdir)
list[2]


# Calling whole functions with source('filepath')
2*x

