setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_07_09_2020")

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

t1 <- read.table('candle.txt', header=TRUE)
t2 <- read.table('sunshine.txt', header=TRUE)

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
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)
# the covariance matrices are quite similar
mcshapiro.test(t1)
mcshapiro.test(t2)
# they are both gaussian

alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: there is statistical evidence that on average they differ


# b) ---------------------------------------------------------------------------

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  
# 3.09412e-08


# c) ---------------------------------------------------------------------------
k <- p
cfr.t <- qt(1-alpha/(2*k),n1 +n2 -2)

# Bonferroni T2 intervals
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)),t1.mean[1]-t2.mean[1], t1.mean[1]-t2.mean[1]+cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)),t1.mean[2]-t2.mean[2], t1.mean[2]-t2.mean[2]+cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)
dimnames(IC.T2)[[2]] <- c('inf','mid','sup')                        
IC.T2

# d) ---------------------------------------------------------------------------

t_diff_candle <- t1$LM2 - t1$LM1
t_diff_sunshine <- t2$LM2 - t2$LM1

shapiro.test(t_diff_candle)
shapiro.test(t_diff_sunshine)


t.test(t_diff_candle, t_diff_sunshine, conf.level=0.95, alternative="greater", 
       mu=0, var.equal=T, paired=F)

#Ho is not reject, so t_diff_candle mean is greater than t_diff_sunshine