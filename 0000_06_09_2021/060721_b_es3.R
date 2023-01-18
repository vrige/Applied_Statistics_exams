setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_06_09_2021")

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

ds <- read.table('boats.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
attach(ds)

n = dim(ds)[1]
p = dim(ds)[2]

# a) ---------------------------------------------------------------------------

# model:
# price = b.0+b.1*dMat+b.2*length+b.3*power +b.4*draught+b.5*crew+b.6*year + eps
# beta.fiberglass = b.1 +b.0
# beta.wood = b.0
material_ <- factor(material)
years_ <- factor(year)
dMat <- ifelse(material_ == "fiberglass",1,0)
dati <- data.frame(ds[,1:6],
                   #year  = years_,
                   dMat = dMat)
dati
fm <- lm(price ~ dMat + length + power + draught + crew + year, data =dati)
summary(fm)

coefficients(fm)
# (Intercept)          dMat        length         power       draught          crew          year 
# -1.063638e+04 -4.373853e+02  3.166172e+02  1.041251e-01  4.297902e+01  6.525758e+02  4.787693e+00

#estimator of the variance 
(residuals(fm)^2)/fm$df

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)
shapiro.test(fm$residuals)$p # ok

vif(fm) 
# b) ---------------------------------------------------------------------------
# testing length, power and draught 
linearHypothesis(fm,
                 rbind(c(0,0,1,0,0,0,0),
                       c(0,0,0,1,0,0,0),
                       c(0,0,0,0,1,0,0)),
                 c(0,0,0))

# p = 2.2e-16 *** -> it has an effect

# c) ---------------------------------------------------------------------------

# testing crew and material
linearHypothesis(fm,
                 rbind(c(0,1,0,0,0,0,0),
                       c(0,0,0,0,0,1,0)),
                 c(0,0))

# p = 2.2e-16 *** -> it has an effect

# d) ---------------------------------------------------------------------------
summary(fm)
fm <- lm(price ~ dMat + length + power + crew + year, data =dati)
summary(fm)
fm <- lm(price ~ dMat + length + power + crew , data =dati)
summary(fm)

coefficients(fm)
# (Intercept)          dMat        length         power          crew 
# -1.019914e+03 -4.301916e+02  3.216404e+02  9.561734e-02  6.485048e+02 

#estimator of the variance 
sum(residuals(fm)^2)/fm$df

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)
shapiro.test(fm$residuals)$p # ok


# e) ---------------------------------------------------------------------------

new_data <- data.frame(length = 10, power = 1070 , dMat= 1, crew = 1)

IP <- predict(fm, newdata=new_data, interval='prediction', level=1-0.05)
IP





