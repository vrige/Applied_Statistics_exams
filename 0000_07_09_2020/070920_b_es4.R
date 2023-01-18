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

# a) ---------------------------------------------------------------------------

noisycurve <- read.table('tide.txt', header=T)
head(noisycurve)
dim(noisycurve)
any(is.na(noisycurve))

n = dim(ds)[1]
p = dim(ds)[2]
m <- 4           # spline order 
degree <- m-1    # spline degree 

nbasis <- 9

Xobs0 <- noisycurve$level
abscissa <- noisycurve$time
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

# it seems that the best number of basis is 12
nbasis= nbasis[which.min(gcv)]

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=nbasis, norder=m)
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df             #  for regression splines the df are the number of basis

# plots
x11()
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

# b) ---------------------------------------------------------------------------

#### Approximate pointwise confidence intervals ####
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']

# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef #estimate the coefficent
Xsp0 <- basismat %*% est_coef #estimated curve
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0-Xobs0)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))

x11()
plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")

# c) ---------------------------------------------------------------------------

Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative from the smooth curve
#Xsp2bis <- eval.fd(abscissa, Xsp$fd, Lfd=2) # second derivative

# compute the central finite differences
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

x11()
plot(abscissa,Xsp1bis ,col="red",lwd=2)
points(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")


# d) ---------------------------------------------------------------------------

# it seems that the first derivative is quite undersmoothed
# however let's try to smooth the derivative using "SMOOTHING SPLINES" 
# (adjusting a lambda parameter for smoothing the derivative)

basis <- create.bspline.basis(abscissa, norder=m)
functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=1e-8)  
# functional parameter, having arguments: 
# basis, order of the derivative to be penalized, smoothing parameter.
Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)

df <- Xss$df   #  the degrees of freedom in the smoothing curve
gcv <- Xss$gcv  #  the value of the gcv statistic

x11()
par(mfrow=c(1,2))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xss0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xss1 ,type="l",col="blue",lwd=2)

# we need to find the right lambda and we can do it with crossvalidation 
# Recommendation: when choosing the smoothing parameter, look at the
# derivatives vs central finite differences

lambda <- 10^seq(-12,-5,by = 0.5)
gcv <- numeric(length(lambda))
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[i])  
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1,1))
plot(log10(lambda),gcv)
lambda[which.min(gcv)]
# lambda = 3.162278e-08

functionalParbest <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[which.min(gcv)])  

Xssbest <- smooth.basis(abscissa, Xobs0, functionalParbest)

Xss0best <- eval.fd(abscissa, Xssbest$fd, Lfd=0)
Xss1best <- eval.fd(abscissa, Xssbest$fd, Lfd=1)

x11()
par(mfrow=c(1,2))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xss0ter ,type="l",col="red",lwd=1)
points(abscissa,Xss0best ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xss1ter ,type="l",col="red",lwd=1)
points(abscissa,Xss1best ,type="l",col="blue",lwd=2)