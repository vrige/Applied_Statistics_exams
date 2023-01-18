setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_16_06_2022")

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


ds <- read.table('discomaniac.txt', header=T)
ds1 <- read.table('lipsticks.txt', header=T)
head(ds)
dim(ds)
dim(ds1)
any(is.na(ds))

ds <- ds[,3:4]
ds1 <- ds1[,3:4]

# a) ---------------------------------------------------------------------------

# we compute the sample of differences
D <- data.frame(price=ds[,1]-ds1[,1], condition=ds[,2]-ds1[,2]) 
D

x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

mcshapiro.test(D)
#  0.7496, so D is gaussian

### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

n <- dim(D)[1]  # 20
p <- dim(D)[2]  #  2

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%
# the two shops don't have the same prices and conditions on the same articles on average

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
# 0.01625523

# b) ---------------------------------------------------------------------------

# our assumption is that D is gaussian and it is
mcshapiro.test(D)
#  0.7496, so D is gaussian

# c) ---------------------------------------------------------------------------
# Ellipsoidal confidence region with confidence level 95%
x11()
plot(D, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=D.mean, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# Ellipsoidal rejecting region with confidence level 95%
ellipse(center=delta.0, shape=D.cov/n, radius=sqrt(cfr.fisher), lwd=2,col="red")

points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.5)
abline(h=delta.0[1], v=delta.0[2], col='grey35')

# d) ---------------------------------------------------------------------------

### Bonferroni intervals
k <- p  # 2
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.mean.price<- c( D.mean[1]-cfr.t*sqrt(D.cov[1,1]/n) , D.mean[1], D.mean[1]+cfr.t*sqrt(D.cov[1,1]/n) )
IC.mean.condition <- c( D.mean[2]-cfr.t*sqrt(D.cov[2,2]/n) , D.mean[2], D.mean[2]+cfr.t*sqrt(D.cov[2,2]/n) )
Bf <- rbind(IC.mean.price, IC.mean.condition)
dimnames(Bf)[[2]] <- c('inf','center','sup')
Bf

# the prices are higher in disc, while the conditions are slightly better in lips
# My suggestion is to go to lips
d.cov <- cov(D)
Bf.var <- cbind(inf = (n-1)*diag(d.cov)/qchisq(1-alpha/(2*k),n-1),
                center = diag(d.cov),
                sup = (n-1)*diag(d.cov)/qchisq(alpha/(2*k),n-1))
Bf.var


