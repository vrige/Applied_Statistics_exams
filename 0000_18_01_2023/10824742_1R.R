setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/000_18_01_2023")

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


t1 <- read.table('white.txt', header=TRUE)
t2 <- read.table('red.txt', header=TRUE)

head(t1)
head(t2)
names(t1)
names(t2)
dim(t1)

n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
p <- dim(t1)[2]

# a) ---------------------------------------------------------------------------
# we want two compare two indipendent gaussians, so we need to check the gaussianity
# and the covariance
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
bartlett.test(t1,t2) #  2.2e-16
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)
# the covariance matrices are quite similar
mcshapiro.test(t1) # 0.4568
mcshapiro.test(t2) # 0.1816
# they are both gaussian

alpha   <- .05
delta.0 <- c(0,0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: there is statistical evidence that e mean of the three variables differ for red and white wines
# b) ---------------------------------------------------------------------------
# the region of rejection is centered in mu0, which is (0,0,0)
delta.0
T2
cfr.fisher
# c) ---------------------------------------------------------------------------
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
#  1.945724e-06
# d) ---------------------------------------------------------------------------

# we want two compare two indipendent gaussians, so we need to check the gaussianity
# and the covariance
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
bartlett.test(t1,t2) #  2.2e-16
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)
# the covariance matrices are quite similar
mcshapiro.test(t1) # 0.4568
mcshapiro.test(t2) # 0.1816
# they are both gaussian

# e) ---------------------------------------------------------------------------

alpha =0.05
k <- p
cfr.t <- qt(1-alpha/(2*k),n1 +n2 -2)

# Bonferroni T2 intervals
IC.B.X1 <- c(t1.mean[1]-t2.mean[1]-cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)),t1.mean[1]-t2.mean[1], t1.mean[1]-t2.mean[1]+cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)) )
IC.B.X2 <- c(t1.mean[2]-t2.mean[2]-cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)),t1.mean[2]-t2.mean[2], t1.mean[2]-t2.mean[2]+cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)) )
IC.B.X3 <- c(t1.mean[3]-t2.mean[3]-cfr.t*sqrt(Sp[3,3]*(1/n1+1/n2)),t1.mean[3]-t2.mean[3], t1.mean[3]-t2.mean[3]+cfr.t*sqrt(Sp[3,3]*(1/n1+1/n2)) )
IC.B <- rbind(IC.B.X1, IC.B.X2,IC.B.X3)
dimnames(IC.B)[[2]] <- c('inf','mid','sup')                        
IC.B

# looking at the three Bonferroni intervals we can notice that both alcohol and density have a similar mean 
# for both types of wine, while acidity is much higher in the red wine.