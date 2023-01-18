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

rm(list=ls())
dat <- read.table('weather.txt', header=TRUE)
head(dat)
names(dat)
dim(dat)

n <- dim(dat)[1]
p <- dim(dat)[2]

# exploration
x11()
boxplot(dat,col='gold')
# we can already observe a different variability among the covariates

x11()
boxplot(scale(x=dat, center = T, scale=F), col='gold')

S <- cov(dat)
round(S,digits = 2)
R <- cor(dat)
round(R,digits = 2)

var.gen <- det(S)
var.gen
var.tot <- sum( diag(S) )
var.tot

##### based on the correlation matrix
dat.sd <- scale(dat)
dat.sd <- data.frame(dat.sd)

x11()
boxplot(dat.sd,col='gold')

head(dat.sd)
m <- sapply(dat.sd,mean)
std <- sapply(dat.sd,sd)
cov(dat.sd)

# PC on correlation matrix
pc.dat <- princomp(dat.sd, scores=T)
pc.dat
summary(pc.dat)
# the first three components explain more than 90% of the variability
# if we need to reduce the model, we can just take the first three


# loadings
load.dat   <- pc.dat$loadings
load.dat

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3)barplot(load.dat[,i], ylim = c(-1, 1))

# let's plot only the most significant loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(ifelse(abs(load.dat[,i]) < 0.3, 0, load.dat[,i]) , ylim = c(-1, 1));abline(h=0)

# The first component is the average of the first 4 features, while the second
# component is just meanWind and Maxwind. Finally, the last one is mostly Humidity

# b) ---------------------------------------------------------------------------

x11()
plot(pc.dat$scores[,1],pc.dat$scores[,2])
plot(1:length(pc.dat$scores[,2]),pc.dat$scores[,2])
plot(1:length(pc.dat$scores[,1]),pc.dat$scores[,1])
# looking at the interpretations of the loadings for the first two components, we can 
# say that the first regards the temperatures and the second one the wind.
# The temperature influences most of the variability of the dataset.
# The wind seems to be quite constant wrt the temperatures. 

# looking also at the other two plots, we can clearly see that the temperature tends
# to increase with time (in fact we are going from Jenuary on), while the wind
# tend to stay constantly strong. Damn milan

# c) ---------------------------------------------------------------------------

# plot of the variability
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.dat$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(dat.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(pc.dat$sdev^2)/sum(pc.dat$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(dat.sd),labels=1:ncol(dat.sd),las=2)

# variance explained 
cumsum(pc.dat$sde^2)[1:3]/sum(pc.dat$sde^2) 

# d) ---------------------------------------------------------------------------

new <- c(MeanTemp = 30,MinTemp = 23,MaxTemp = 36,DewPoint=22,
         Humidity = 65, Visibility = 19 , MeanWind = 5, MaxWind = 15)
load.dat <- pc.dat$loadings[,1:3]

m <- sapply(dat,mean)
var <- sapply(dat,var)
new_s <- (new-m)/(sqrt(var))
scores.aug1 <- t(load.dat)%*%(new_s)
t(scores.aug1)
# Comp.1   Comp.2    Comp.3
# 3.548411 0.847291 0.9029056


