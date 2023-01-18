setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_06_07_2021")

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

ds <- read.table('chicca.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)

# a) ---------------------------------------------------------------------------

# mean for a multivariate Gaussian

n <- dim(ds)[1]
p <- dim(ds)[2]

ds.mean   <- sapply(ds,mean)
ds.cov    <- cov(ds)
ds.invcov <- solve(ds.cov) #solve computes the inverse matrix


#_______________________________________________________________________________
### Test for the mean of a multivariate Gaussian 

################################################################################
# Premise: general rule to perform a test
# 1)  Formulate the test (and test the Gaussian assumption, if needed)
# 2)  Compute the test statistics 
# 3a) Having set the level of the test, verify whether the test statistics 
#     belongs to the region of rejection (i.e., if there is statistical  
#     evidence to reject H0)
# 3b) Compute the p-value of the test
################################################################################

#_______________________________________________________________________________
### Test on the mean of level alpha=1%
### H0: mu == mu0 vs H1: mu != mu0
### with mu0=c(0,90)
###-----------------------------------
mcshapiro.test(ds) # it's gaussian

alpha <- 0.01
mu0 <- c(0,90)

# T2 Statistics
ds.T2 <- n * (ds.mean-mu0) %*% ds.invcov %*% (ds.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
ds.T2 < cfr.fisher  #false
# there is statistical evidence to reject H0 at level 1%
# Rejection region: {ds.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

# plot of the statistic
x11()
xx <- seq(0,40,by=0.05)
plot(xx,df(xx*(n-p)/((n-1)*p),p,n-p),type="l",lwd=2,main='Density F(p,n-p)',xlab='x*(n-p)/((n-1)*p)',ylab='Density')
abline(h=0,v=ds.T2*(n-p)/((n-1)*p),col=c('grey','red'),lwd=2,lty=c(2,1))
abline(h=0,v=qf(1-alpha,p,n-p),col=c('grey','blue'),lwd=2,lty=c(2,1))

# Region of rejection (centered in mu0)
x11()
plot(ds, asp = 1)
ellipse(mu0, shape=ds.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
# or with the qchisq
# remember that we can do the test with n small or n large (both leading to the same conclusion)
cfr.chisq <- qchisq(1-alpha,p)
ellipse(mu0, shape=ds.cov/n, sqrt(cfr.chisq), col = 'red', lty = 2, center.pch = 16)

# We add a red point in correspondence of the sample mean
points(ds.mean[1], ds.mean[2], pch = 16, col ='red', cex = 1.5)

# Confidence region (centered in ds.mean)
# { m \in R^2 s.t. n * (ds.mean-m)' %*% (ds.cov)^-1 %*% (ds.mean-m) < cfr.fisher }
ellipse(ds.mean, ds.cov/n, sqrt(cfr.fisher), col = 'orange', lty = 2, lwd=2, center.cex=1)
points(mu0[1], mu0[2], pch = 16, col ='red', cex = 1.5)

# b) ---------------------------------------------------------------------------

# Compute the p-value 
P <- 1-pf(ds.T2*(n-p)/((n-1)*p), p, n-p)
P
#4.304335e-13

# c) ---------------------------------------------------------------------------
k <- p # number of intervals I want to compute (set in advance)
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf <- cbind(inf = ds.mean - cfr.t*sqrt(diag(ds.cov)/n),
            center = ds.mean, 
            sup = ds.mean + cfr.t*sqrt(diag(ds.cov)/n))
Bf

#The bonferroni interval for the mean of the delay does not contain the value 0, so this supports our decision
#to reject Ho at level 1%; while with bonferroni at level 99% we are not in the position to say that the stay
#is significantly different from 90

# d) ---------------------------------------------------------------------------

# we can test it as a linear combination of gaussians (the sum in this case)
a = c(1,1)
alpha <- 0.1
T2 = sqrt(n) * ( ds.mean %*% a - mu0 %*% a ) /(sqrt(a %*% ds.cov %*% a))
T2
cfr.student = qt(1-alpha,n-1)
cfr.student
abs(T2)< cfr.student # FALSE, reject null hypothesis
# we have statistical evidence that the turns don't last 90 minutes on average

#p value
p = pt(1-T2,n-1) + pt(-T2,n-1) # or pt(1-T2,n-1) * 2
p
