setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_12_07_2022")

library(car)                 # plotting ellipses, Box-Cox transformation, linearHypothesis()
library(MASS)                # lda(), qda() function, lm.ridge()
library(class)               # knn function
library(glmnet)              # glmnet() function (Ridge, Lasso, Elastic Net)
library(leaps)               # regsubsets()
library(tree)                # tree() function (classification and regression tree
library(mvtnorm)
library(mvnormtest)
library(rgl)
library(spam)
library(fields)
library(Matrix)
library(nlmeU)
library(corrplot)
library(nlme)
library(lattice)
library(plot.matrix)
library(lme4)
library(insight)
library(ggplot2)
library(sp)  
library(gstat)
library(fda)
library(KernSmooth)
library(fdakma)
library(e1071)
load("../Labs/Lab 5/mcshapiro.test.RData")


ds <- read.table('demogorgons.txt', header=T)
head(ds)
names(ds)
dim(ds)
any(is.na(ds))

n = dim(ds)[1]
p = dim(ds)[2]

misc <- sample(n)
ds <- ds[misc,] #permutation of the data

# a) ---------------------------------------------------------------------------
#### COMPUTING DISTANCE
###-------------------
ds_dist <- dist(ds, method='euclidean')

#### CLUSTERING
###-------------------
ds_co <- hclust(ds_dist, method='average')

#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,1))
plot(ds_co, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_co, k=2)

#### CUTTING
###-------------------
cluster.es <- cutree(ds_co, k=2) # euclidean-complete
clus_1 <- ds[cluster.es==1,]
clus_2 <- ds[cluster.es!=1,]

# b) ---------------------------------------------------------------------------
i1 <- cluster.es==1
i2 <- cluster.es!=1
fact <- factor(cluster.es)
g <- 2
p <- 2

### Model: X.ij = mu + tau.i + eps.ij; eps.ij~N_p(0,Sigma), X.ij, mu, tau.i in R^4
### Test:
### H0: tau.1 = tau.2  = (0,0)'
### H1: (H0)^c
### that is
### H0: The membership to a cluster hasn't any significant effect on the mean
###     of X.ij 
### H1: There exists at least one direction in R^4 along which the two clusters have different features


### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
for(i in 1:g)
  Ps <- c(Ps, mcshapiro.test(ds[get(paste('i',i, sep='')),1:p])$p) 
Ps
# 0.8380 0.6064, they are both normal

# 2) same covariance structure (= same covariance matrix Sigma)
S  <-  cov(ds)
S1 <-  cov(ds[i1,])
S2 <-  cov(ds[i2,])
S1
S2
bartlett.test(ds[,1:2],cluster.es)
#  0.5642
# they are quite similar

fit <- manova(as.matrix(ds) ~ fact)
summary.manova(fit,test="Wilks")
# 2.2e-16, reject Ho, fact has an effect

summary.aov(fit)
# it has an effect on both long and lat


# c) ---------------------------------------------------------------------------


#### CENTROIDS
###-------------------
# centroids <- apply (geisha, 2, function (x) tapply (x, suc, mean))
m1 <- sapply(clus_1, mean)
m2 <- sapply(clus_2, mean)

# Estimate of variances
W <- summary.manova(fit)$SS$Residuals/fit$df.residual 
W2 <- diag(t(fit$res) %*% fit$res)/(n-g) 

# plot
plot(ds[,1],ds[,2],col=cluster.es)
points(m1[1],m1[2],col="green")
points(m2[1],m2[2],col="green")


D1 <- ds[i1,]
cfr.fisher1 <- ((n1-1)*p/(n1-p))*qf(1-alpha,p,n1-p)
D2 <- ds[i2,]
cfr.fisher2 <- ((n2-1)*p/(n2-p))*qf(1-alpha,p,n2-p)

D.mean1   <- sapply(D1,mean)
D.cov1    <- cov(D1)
D.invcov1 <- solve(D.cov1)

D.mean2  <- sapply(D2,mean)
D.cov2    <- cov(D2)
D.invcov2 <- solve(D.cov2)

# plot of D1
x11()
plot(D1, asp=1, pch=1,col="blue", main='Dataset of the Differences')
ellipse(center=D.mean1, shape=D.cov1/n, radius=sqrt(cfr.fisher1), lwd=2)

# plot of D2
x11()
plot(D2, asp=1, pch=1,col="red", main='Dataset of the Differences')
ellipse(center=D.mean2, shape=D.cov2/n, radius=sqrt(cfr.fisher2), lwd=2)

# complete plot
plot(ds[,1],ds[,2],col=cluster.es)
points(m1[1],m1[2],col="green")
points(m2[1],m2[2],col="green")
ellipse(center=D.mean1, shape=D.cov1/n, radius=sqrt(cfr.fisher1), lwd=2)
ellipse(center=D.mean2, shape=D.cov2/n, radius=sqrt(cfr.fisher2), lwd=2)
