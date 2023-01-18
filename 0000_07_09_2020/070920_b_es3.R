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


t1 <- read.table('leaven.txt ', header=TRUE)

head(t1)
names(t1)
dim(t1)

n = dim(t1)[1]
p = dim(t1)[2]

attach(t1)
t1[,3] <- factor(t1[,3])
dYeast <- ifelse(t1[,3] == "by",1,0)
# a) ---------------------------------------------------------------------------

dati <- data.frame(t1[,1:2],time_2 = time^2, dYeast  = dYeast)
# model: y = b.0 + b.1 * time + b.2 * time * dYeast + b.3 * time^2 + b.4 * time^2 * dYeast
# beta.by.time = b.1 + b.2        beta.by.time_2 = b.3 + b.4
# beta.sd.time = b.1              beta.by.time_2 = b.3
fit <- lm(volume ~ time + time:dYeast + time_2 + time_2:dYeast, data=dati)
summary(fit)

vif(fit) # collinearity!

# model assumptions:
# - gaussianity:
shapiro.test(fit$residuals)$p 
# 0.928366

#estimator of the variance 
sum(residuals(fit)^2)/fit$df
# 0.002207288 

coefficients(fit)
# (Intercept)          time        time_2   time:dYeast dYeast:time_2 
# 0.9839522351  0.0670422710 -0.0005003801 -0.0529347109  0.0122343277 

# b) ---------------------------------------------------------------------------

linearHypothesis(fit, rbind(c(0,0,0,0,1), c(0,0,0,1,0)), c(0,0)) 
# 2.2e-16 reject ho, there is a dependency on the kind of yeast

linearHypothesis(fit, rbind(c(0,0,0,0,1)), c(0)) 
# 2.2e-16 reject ho, there is a dependency on the second grade for the "by"

linearHypothesis(fit, rbind(c(0,0,1,0,0)), c(0)) 
# 0.5806 don't reject H0, there is no dependency on a second grade for "sd"
# => - there is statistical evidence that the degree of the polynomial for 
# the brewer's yeast is higher than the degree of the polynomial for the sourdough.

# c) ---------------------------------------------------------------------------

fit <- lm(volume ~ time + time:dYeast + time_2 + time_2:dYeast, data=dati)
summary(fit)

fit <- lm(volume ~ time + time:dYeast + time_2:dYeast, data=dati)
summary(fit)

# model assumptions:
# - gaussianity:
shapiro.test(fit$residuals)$p 
# 0.9747498

#estimator of the variance 
sum(residuals(fit)^2)/fit$df
# 0.002187187

coefficients(fit)
# (Intercept)          time   time:dYeast dYeast:time_2 
# 0.99239281    0.06208154   -0.05098783    0.01196397 

# d) ---------------------------------------------------------------------------
# i would suggest the one with yeast "by" because the volume has a quadratic
# dependence on time


Z0.new <- data.frame(time=2, time_2=2^2,dYeast=1)

# Conf. int. for the mean
Conf <- predict(fit, Z0.new, interval='confidence', level=1-0.01)  
Conf

# fit      lwr      upr
# 1 1.062436 1.037311 1.087562



