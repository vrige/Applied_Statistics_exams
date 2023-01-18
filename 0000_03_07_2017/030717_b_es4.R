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

ds <- read.table('garden.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)

# a) ---------------------------------------------------------------------------
# E = â0 + â1 · x1 + â2 · x2 + â3 · x3 + â4 · x4 + å,
fm <- lm(extension ~ carps + maple + cherry + stones )
summary(fm) 

coefficients(fm)
sum(residuals(fm)^2)/fm$df

x11()
par(mfrow = c(2,2))
plot(fm) # they seem ok

# collinearity
vif(fm)

# verify assumptions
shapiro.test(residuals(fm))

# b) ---------------------------------------------------------------------------
linearHypothesis(fm,rbind(c(0,0,1,0,0), c(0,0,0,1,0)), c(0,0))
# p value = 2.2e-16   -> they have an effect

linearHypothesis(fm,rbind(c(0,1,0,0,0), c(0,0,0,0,1)), c(0,0))
# p value = 3.822e-15 -> they have an effect


# c) ---------------------------------------------------------------------------

fm <- lm(extension ~ carps + maple + cherry + stones )
summary(fm) 

fm <- lm(extension ~ carps + maple + cherry  )
summary(fm) 

fm <- lm(extension ~ carps + cherry  )
summary(fm) 

# d) ---------------------------------------------------------------------------

coefficients(fm)
sum(residuals(fm)^2)/fm$df

x11()
par(mfrow = c(2,2))
plot(fm) # they seem ok

vif(fm)
# no more collinearity!