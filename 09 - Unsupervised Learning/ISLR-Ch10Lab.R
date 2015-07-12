### ISLR Chapter 10 Lab

## Lab 1: Prinicipal Components Analysis
rm(list=ls())

states = row.names(USArrests)
states
names(USArrests)
mu = apply(USArrests, MARGIN=2, FUN=mean)
sigma = sqrt(apply(USArrests, MARGIN=2, FUN=var))
# Different means and variances. 
# Should standardize so that thing with largest variables (Assault) doesn't automatically dominate.

pr.out = prcomp(USArrests, scale=TRUE) # scale=TRUE scaled all variables to sigma=1. center=TRUE would set mu=0.
summary(pr.out)

# We could have done this by hand:
# Step 1: Standardize variables

stdArrests = USArrests/matrix(rep(sigma,50),nrow=50, byrow=TRUE)
#stdArrests = apply(USArrests, 2, FUN=scale, scale=TRUE, center=FALSE)
apply(stdArrests, 2, mean)
apply(stdArrests, 2, var)

# Step 2: Run PCA without scaling.
pr.out2 = prcomp(stdArrests, scale=FALSE)
summary(pr.out2)
rm(pr.out2)

names(pr.out)
pr.out$rotation # PC loadings (MxP)
# Called rotation matrix (R) because X*R = Z, where Z is the NxM matrix of PC scores for each PC.
# ASSUMING X HAS ALREADY BEEN SCALED TO BE MEAN ZERO AND VAR 1!!!!
as.matrix(scale(USArrests))%*%pr.out$rotation
# We can recover Z through the above formula, or directly from pr.out:
pr.out$x

as.matrix(stdArrests)%*%pr.out$rotation == pr.out$x

biplot(pr.out, scale=0)
# This looks like a flipped version of the plot in the 
# Recall that the PCs are unique up to a sign change (since if theta maximizes Var(theta*x), so does -theta).
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)

pr.out$center # mean of unscaled variables
apply(USArrests, 2, mean) # same
pr.out$scale # scale of unscaled variables
apply(USArrests, 2, sd) # same

pr.out$sdev # std deviations of principal components
pr.var = pr.out$sdev^2 # total variance explained by each principal component
pve = pr.var/sum(pr.var) # total variance explained = sum of variance explained by each PC

par(mfrow=c(1,2))
plot(pve, xlab="PC", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="PC", ylab="Cum. Proportion of Variance Explained", ylim=c(0,1), type='b')


## Lab 2: Clustering
set.seed(2)
x = matrix(rnorm(50*2), ncol=2)
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] - 4
y = c(rep(2,25),rep(1,25))

par(mfrow=c(1,1))
plot(x, col=2+y)

# K-means clustering
km.out = kmeans(x, centers=2, nstart=20) # centers= # of clusters. nstart= # of repetitions to ensure convergence
km.out$cluster
y
# perfect separation!
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
# Note that if x were multidimensional, we could have simply found the first 2 PC scores and plotted those.

# What if we had assumed 3 clusters?
set.seed(4)
km.out = kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

# Note, if we had done nstart=1, we would likely not have gotten the same cluster. 
# We probably would have gotten a worse within-cluster sum of squares 
# (which is the objective function in K-means clustering).
set.seed(3)
km.out = kmeans(x,3,nstart=1)
km.out$tot.withinss

km.out = kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering
# hclust uses 2 main imputs: d= dissimilarity measure. method= linkage method.
hc.complete = hclust(d=dist(x), method="complete")
hc.average = hclust(d=dist(x), method="average")
hc.single = hclust(d=dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=0.9)

cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2) # single is a bit unbalanced, putting one obs into its own cluster, and the rest into another.

cutree(hc.single, 4) # still a problem

# By the way, note that the clustering may depend on scaling.
xsc = scale(x)
par(mfrow=c(1,2))
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
plot(hc.complete, main="Complete Linkage", xlab="", sub="", cex=0.9)

# Now let's use the correlation metric. Note that with 2 features, the correlation is always 1, so we need a third feature.
x = matrix(rnorm(30*3), ncol=3)
#as.dist() converts a dissimilarity matrix into a distance structure, usable by hclust()
# We will try using the correlation of variables as our dissimilarity matrix.
# That is, we group things together with a correlation near 1 (or -1) (i.e. |1-cor| near 0.. 
# take the negative of the correlation
cor(t(x)) 
1-cor(t(x)) 
# note that since cor is in [-1,1], 1-cor is in [0,2], where 0 corresponds to cor=1 and 2 corresponds to cor= -1.
# So observations are most similar when they are strongly positive correlated 
# and most dissimilar when they are strongly negatively correlated.
# Zero correlation is in the middle.
# cor(x) returns the correlation of the 2 columns, but we want to correlation of the 50 obs.
# We can get this by tricking cor() into thinking the observations are variables by transposing x, so it's 2x50.

dd = as.dist(1-cor(t(x)))
par(mfrow=c(1,1))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

## Lab 3: NCI60 Data Examples
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data
dim(nci.data)
table(nci.labs)
# Lots of variables, few observations (N=64, P=6,830)
# Each row is a cancer lines. Each variable is a gene expression measurement.
# The goal is to use PCA clustering to group cancer types. We then want to see
# how well the unsupervised clustering matches with the true cancer types.
# For example, does it group all the breast cancers together? If so, we probably
# have a good model.
nci.labs[1:4]
nci.labs

pr.out = prcomp(nci.data, scale=TRUE)
# Let's do PCA and plot the component scores. We will associate each cancer line
# with a color. If the plotted colors are similar to its neighbors
Cols = function(vec) {
        # pass in a list of discrete values.
        cols = rainbow(length(unique(vec))) 
        # rainbow creates a vector of colors. E.g. rainbow(4) creates a vector of 4 colors that span a rainbow.
        return(cols[as.numeric(as.factor(vec))])
        # return a vector showing the color corresponding to the values in the input.
        # e.g., input=("a", "b", "a", "a","c") would return (color1, color2, color1, color1, color3)
}
#Cols(c("a","b","a","a","c"))

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2") # first 2 PC scores
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3") # first & third PC scores
# Yes: similar cancer genes tend to have similar PC scores (at least for the first few PCs)
summary(pr.out)
plot(pr.out) # plots squared std deviation of PCs for first few PCs. Same as:
#barplot(pr.out$sdev[1:9]^2) 
# What's the cum PVE look like?
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Prinicipal Component", col="Blue")
plot(cumsum(pve), type="o", ylab="Cum. PVE", xlab="Prinicipal Component", col="brown3")

# Hierarchical Clustering
sd.data=scale(nci.data)
data.dist = dist(sd.data) # computes distance matrix between all the the different observations. (N-1)x(N-1).

par(mfrow=c(1,3))
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", ylab="", sub="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", ylab="", sub="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="", ylab="", sub="")
# Pretty good. The leukemias tend to be grouped together. Same for the melanomas.

hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out,4)
table(hc.clusters, nci.labs)
# cluster 1 is big, but it nests all melanoma, all ovarian, and all renal. cluster 3 is mostly leukemia. Cluster 4 is mostly colon.
# In the graph, from left to right, the first branch is cluster 2. The second branch is cluster 1. The third is 4. The last is 3.

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out

# Recall that K-clustering can give very different results than hierarchical (since it does not assume a hierarchical structure)
set.seed(2)
km.out = kmeans(sd.data, 4, nstart=20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters) # compare KM to HC
# Different. One cluster is the same (cluster 2 in KM is the same group as cluster 3 in HC)
# But thre rest are different. HC's cluster 1 is split by KM into three groups. 

# To reduce dimensionality and make the results easier to interpret, we can
# simply clust using the first few (say 5) PCAs.
hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)
# Nowe the clusters are more divided, but still over-aggregate in some senses.
# NSCLC, Colon, and Renal share a group, but have few misclassifications.
# Same for melanoma & breast cancer.