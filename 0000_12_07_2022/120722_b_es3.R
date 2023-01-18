setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_12_07_2022")

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


ds <- read.table('rent.txt', header=T)

names(ds)
dim(ds)
any(is.na(ds))
head(ds)

n = dim(ds)[1]
p = dim(ds)[2]

attach(ds)

# a) ---------------------------------------------------------------------------
two_bath <- factor(ds[,9])
dBath <- ifelse(two_bath == TRUE,1,0)
ds2 <- as.data.frame(cbind(ds[,1:8],dBath))

fit <- lm(price ~ .*dBath, data = ds2)
summary(fit)

coefficients(fit)
# (Intercept)           footage               age        renovation         transport 
# 2950.82578582       23.86056223      -10.04728859       -7.99891771        0.70526444 
# center       supermarket              park             dBath     footage:dBath 
# -0.11015755       -0.25634206       -0.17230360       76.45448500        1.64253808 
# age:dBath  renovation:dBath   transport:dBath      center:dBath supermarket:dBath 
# 15.38915966       10.69972492       -0.88497751        0.03612037        0.52644766 
# park:dBath 
# -0.18265410 
residuals(fit)

# estimate of sigma^2
sum(residuals(fit)^2)/fit$df  

# b) ---------------------------------------------------------------------------

plot(fit)

vif(fit)

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)
# we can check it thorugh the residuals
shapiro.test(residuals(fit))
#p-value = 0.653, it's normal

# c) ---------------------------------------------------------------------------
# Build the matrix of predictors
x <- model.matrix(price ~ .*dBath, data = ds2)[,-1]
# Build the vector of response
y <- price
fit.lasso <- glmnet(x,y, lambda = 45)

coef.lasso <- predict(fit.lasso,s =45,type = 'coefficients')
coef.lasso 

selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]
selected

# d) ---------------------------------------------------------------------------

range = 1: 100 
cv.lasso <- cv.glmnet(x,y, lambda = range)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

fit.lasso <- glmnet(x,y, lambda = bestlam.lasso)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso,s =bestlam.lasso,type = 'coefficients')
coef.lasso 

selected =  as.data.frame(t(coef.lasso[which(abs(coef.lasso)> 0.01)]))
selected
#names(selected) <- coef.lasso@Dimnames[1][which(abs(coef.lasso)> 0.01)]
# e) ---------------------------------------------------------------------------

data.new <- data.frame(footage=30,age=5,renovation=5,transport=300,center=1000,
                       supermarket=500,park=100,two.bathrooms=FALSE, footage.two.bathroomsTRUE = 0,
                       age.two.bathroomsTRUE = 0,
                       renovation.two.bathroomsTRUE = 0,
                       transport.two.bathroomsTRUE = 0,
                       center.two.bathroomsTRUE = 0,
                       supermarket.two.bathroomsTRUE = 0,
                       park.two.bathroomsTRUE = 0)

predict(fit.lasso, newx=as.matrix(data.new), s=bestlam.lasso, type = 'response')

### OPPURE (piu semplice)
data.new <- data.frame(inter = 1,footage=30,age=5,renovation=5,transport=300,center=1000,
                       supermarket=500,park=100)
price <- coef.lasso[1:8]%*%t(as.matrix(data.new))
price






