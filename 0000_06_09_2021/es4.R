setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 2/000_06_09_2021")

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
library(e1071)
load("../Labs/Lab 5/mcshapiro.test.RData")


# The file wind.txt contains hourly measurements of wind intensity in Porto Cervo 
# recorded for the last 30 days. Consider a functional data analysis approach where, 
# for each day, the measurements provided are considered as discrete sampling of 
# underlying smooth functions.

data <- read.table('wind.txt', header=T)
head(data)
attach(data)

colnames(data)

dim(data)
n <- dim(data)[1]
p <- dim(data)[2]

# 1 - Perform a smoothing of each daily data through a projection over a B-spline
# basis with 12 basis elements of degree 2. Provide a plot of the smoothed data
# and report the first 3 coefficients obtained for Day 1.


data <- as.matrix(data)
data <- t(data)   # NOTICE THIS!!
head(data)
dim(data)
matplot(data,type='l',main='Porto Cervo',xlab='Day',ylab='Wind')
degree = 2
order = degree + 1
time <- 1:24

basis <- create.bspline.basis(rangeval=c(0,24),nbasis=12,norder=3)
data_L.fd <- Data2fd(y = data,argvals = time ,basisobj = basis)
plot.fd(data_L.fd, main="B-splines")
data_L.fd$coefs[1:3,1]
#bspl3.1  bspl3.2  bspl3.3 
#21.92529 12.41474 28.44647 

# 2 - Perform a functional principal component analysis of the smoothed data obtained 
# at point (a). Report the variance explained along the first 3 functional principal
# components, the screeplot and a plot of the first 3 eigenfunctions.

pca_W.1 <- pca.fd(data_L.fd,nharm=5,centerfns=TRUE)

# scree plot
# CPV = Percentage of total Variance
plot(pca_W.1$values[1:12],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:12]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

# first three FPCs
x11()
layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')
abline(h=0,lty=2)

# The interpretations can be the following ones:
# the first one is the average( all positive), the second is a contrast between the morning
# and night hours and the last one is a constrast between night and day hours


# 3 -  Propose a possible dimensionality reduction for the data and justify your 
# choice. Plot the retained principal components as perturbation of the mean. 
# Interpret the retained principal components

par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)


#first - weighted average with heavier weights in the central part of the day 
#second - contrast between first 15 hour of the day and the rest of the evening 

# the first component explains almost 90% of the variability (looking at the scree
# plot). It should be enough
media <- mean.fd(data_L.fd)

plot(media,lwd=2,ylim=c(-5,40),ylab='temperature',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
# there is much more variation in amplitude during the day hours 


# 4 - Provide a plot of the scores along the first two principal components, 
# highlight in the plot the point corresponding to Day 1 and comment on the
# characteristics of Day 1.

par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
points(pca_W.1$scores[1,1],pca_W.1$scores[1,2],col=2, lwd=4)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2",xlim=c(-400,250))
text(pca_W.1$scores[,1],pca_W.1$scores[,2],dimnames(data)[[2]], cex=1)

#day 1 has high value of wind intensity in the first part of the day and generally 
#an above the mean measurement of wind speed throughout the day (especially around half of the day) 



