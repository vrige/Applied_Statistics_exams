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

ds <- read.table('pc.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)
OS_ <- factor(OS)
n = dim(ds)[1]
p = dim(ds)[2]

# a) ---------------------------------------------------------------------------
# Y = á + âg · f + ãg · c + å
# Equivalent Model:
# price = b.0+b.1*cache_acc+b.2*DMac*cache_acc+b.3*dWindows*cache_acc 
#         +b.4*freq+b.5*DMac*freq+b.6*dWindows*freq + eps
#
# âeta.mac = b.1 + b.2         ã.mac = b.4 + b.5
# âeta.windows = b.1 + b.3     ã.windows = b.4 + b.6
# âeta.linux = b.1             ã.linux = b.4
 
dMac = ifelse(OS_ == "Mac",1,0)
dWindows = ifelse(OS_ == "Windows",1,0)
dati <- data.frame(price  = price,
                   freq   =  freq,
                   cache_acc= cache_acc,
                   dMac  = dMac,
                   dWindows = dWindows)
dati
fm <- lm(price ~ cache_acc + cache_acc:dMac + cache_acc:dWindows + freq +
           freq:dMac + freq:dWindows , data =dati)
summary(fm)

coefficients(fm)
# (Intercept)          cache_acc               freq     cache_acc:dMac cache_acc:dWindows 
# 1345.656963          -2.588699          34.622543          -1.650034          -6.413569 
# dMac:freq      dWindows:freq 
# 86.590317          41.579981

#estimator of the variance 
sum(residuals(fm)^2)/fm$df

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)
shapiro.test(fm$residuals)$p # ok

# b) ---------------------------------------------------------------------------
x11()
par(mfrow=c(2,2)) #omoschedastic residuals, zero  mean
plot(fm)

shapiro.test(residuals(fm)) #normality assumptions to perform tests 
#p = 0.1274

# OS
linearHypothesis(fm,
                 rbind(c(0,0,0,1,0,0,0),
                       c(0,0,0,0,1,0,0),
                       c(0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,1)),
                 c(0,0,0,0))
# 1.903e-08 ***
# OS has an effect

# c) ---------------------------------------------------------------------------
# cache access
linearHypothesis(fm,
                 rbind(c(0,1,0,0,0,0,0),
                       c(0,0,0,1,0,0,0),
                       c(0,0,0,0,1,0,0)),
                 c(0,0,0))
# p = 0.36964 so at 10% we dont'reject the null hypthesis that all 
# cache access doesn't have an effect

# d) ---------------------------------------------------------------------------
fm <- lm(price ~  freq + freq:dMac + freq:dWindows , data =dati)
summary(fm)

fm <- lm(price ~  freq + freq:dMac  , data =dati)
summary(fm)

vif(fm) # no collinearity
# e) ---------------------------------------------------------------------------

new_data <- data.frame(freq = 3.2, cache_acc = 10 , dMac= 0, dWindows = 1)

IP <- predict(fm, newdata=new_data, interval='confidence', level=1-0.1)
IP


