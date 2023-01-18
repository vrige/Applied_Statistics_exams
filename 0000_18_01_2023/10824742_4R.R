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


ds <- read.table('cocktails.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))

attach(ds)
n <- dim(ds)[1]
p <- dim(ds)[2]
detach(ds)

# a) ---------------------------------------------------------------------------

#ds <- as.matrix(ds)
#ds <- t(ds)

matplot(ds,type='l',main='Cocktails',xlab='Day',ylab='Measurements')
nbasis = 27
dim(ds)
time<- seq(1,52,by=1)

basis <- create.fourier.basis(rangeval=c(0,52),nbasis=nbasis)
data_L.fd <- Data2fd(ds,time,basis)
plot.fd(data_L.fd, main="Fourier")

data_L.fd$coefs[1:3,1]
# const       sin1       cos1 
# 146.024827   7.178419 -89.349090
# b) ---------------------------------------------------------------------------

pca_W.1 <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)
pca_W.1$values[1:5]
cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)

# scree plot
# CPV = Percentage of total Variance
plot(pca_W.1$values[1:12],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:12]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

# c) ---------------------------------------------------------------------------


# first three FPCs
x11()
layout(cbind(1))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)

# Clearly the first component alone explains almost 95% of all the variability
# of the dataset, so we can just take the first one.

# d) ---------------------------------------------------------------------------

media <- mean.fd(data_L.fd)

plot(media,lwd=2,ylim=c(-5,42),ylab='y',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)

# looking at the plot we can say that the first principal component is a weigthed average
# of consumation during the all year. However, there is much more amplitude variation 
# on the peaks of the graph, so during the summer and the brief winter holidays. 

# e) ---------------------------------------------------------------------------

par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
points(pca_W.1$scores[1,1],pca_W.1$scores[1,2],col=2, lwd=4)

points(pca_W.1$scores[2,1],pca_W.1$scores[2,2],col=2, lwd=4)
points(pca_W.1$scores[3,1],pca_W.1$scores[3,2],col=2, lwd=4)


