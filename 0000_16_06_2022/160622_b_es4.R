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


noisycurve <- read.table('listening.txt', header=T)
noisycurve <- t(noisycurve)
head(noisycurve)
names(noisycurve)
dim(noisycurve)
any(is.na(noisycurve))

n = dim(noisycurve)[1]
p = dim(noisycurve)[2]
m <- 3           # spline order 
degree <- m-1    # spline degree 

# a) ---------------------------------------------------------------------------
x11()
matplot(noisycurve,type='l',main='Number',ylab='listening',xlab='day')

Xobs0 <- noisycurve
abscissa <- 1:365
NT <- length(abscissa)

# generalized cross-validation CV
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
nbasis = nbasis[which.min(gcv)]

basis= nbasis[which.min(gcv)]
basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=nbasis, norder=m)


functionalPar <- fdPar(fdobj=basis, lambda=100)  

Xss <- smooth.basis(abscissa, Xobs0, functionalPar)
Xss0bis <- eval.fd(abscissa, Xss$fd, Lfd=0)

x11()
matplot(abscissa,Xss0bis ,type="l",col="green",lwd=1)

data_W.fd.1 <- Data2fd(y = Xss0bis,argvals = abscissa,basisobj = basis)
x11()
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1]
# bspl3.1  bspl3.2  bspl3.3 
# 17.93336 18.87656 18.50190

# b) ---------------------------------------------------------------------------

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)
pca_W.1$values[1:5]

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=131 are non-null
x11()
plot(pca_W.1$values[1:14],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:14]/sum(pca_W.1$values),xlab='j',ylab='CPV')
     
cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)

# c) ---------------------------------------------------------------------------


# we take just the first two functional principal components


# d) ---------------------------------------------------------------------------

# first two FPCs
x11()
layout(cbind(1,2))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)

# luglio ed un massimo tra febbraio e marzo
x11()
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

# e) ---------------------------------------------------------------------------

# Scores
par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
points(pca_W.1$scores[1,1],pca_W.1$scores[1,2],col=2, lwd=4)



