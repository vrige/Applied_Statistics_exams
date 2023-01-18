setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_03_07_2017")

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

ds <- read.table('bento.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)

ds1 <- ds[,1:4]
ds2 <- ds[,5:8]

D <- data.frame(rice=ds1[,1]-ds2[,1], sashimi=ds1[,2]-ds2[,2],vegetables=ds1[,3]-ds2[,3],okashi=ds1[,4]-ds2[,4]) 
D

x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

dev.off()

# a) ---------------------------------------------------------------------------

### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

# Test the Gaussian assumption (on D!)
mcshapiro.test(D) # 0.94
# it's normal

n <- dim(D)[1]  # 11
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

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
# reject H0 at 5% 

# b) ---------------------------------------------------------------------------

# Now, let's communicate our results to the client.
# Let's build confidence intervals for linear combination of the
# components of the mean vector

### Simultaneous T2 intervals
IC.T2_1 <-  c( D.mean[1]-sqrt(cfr.fisher*D.cov[1,1]/n) , D.mean[1], D.mean[1]+sqrt(cfr.fisher*D.cov[1,1]/n) )
IC.T2_2  <- c( D.mean[2]-sqrt(cfr.fisher*D.cov[2,2]/n) , D.mean[2], D.mean[2]+sqrt(cfr.fisher*D.cov[2,2]/n) )
IC.T2_3 <-  c( D.mean[3]-sqrt(cfr.fisher*D.cov[3,3]/n) , D.mean[3], D.mean[3]+sqrt(cfr.fisher*D.cov[3,3]/n) )
IC.T2_4  <- c( D.mean[4]-sqrt(cfr.fisher*D.cov[4,4]/n) , D.mean[4], D.mean[4]+sqrt(cfr.fisher*D.cov[4,4]/n) )

T2 <- rbind(IC.T2_1, IC.T2_2,IC.T2_3,IC.T2_4)
dimnames(T2)[[2]] <- c('inf','center','sup')
dimnames(T2)[[1]] <- c('rice','sashimi ','vegetables','okashi')
T2

# we can say that there is evidences that families are consuming more dishes with Hanami
# when it comes to sashimi and okashi. We cannot say much about rice and vegetables