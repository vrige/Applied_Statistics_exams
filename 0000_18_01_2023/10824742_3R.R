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


ds <- read.table('wineReviews.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))

attach(ds)
n <- dim(ds)[1]
p <- dim(ds)[2]
region_ <- factor(region)
region_
names(ds)
#detach(ds)

# a) ---------------------------------------------------------------------------
# points = ??0 + ??1 · price + ??2 · alcohol + ??
# ln (points) = ??0 + ??1 · ln (price) + ??2 · alcohol + ??
pairs(ds[,1:3])
pairs(as.data.frame(cbind(log(ds[,1]),log(ds[,2]),ds[,3])))

fit1 <- lm(points ~ price + alcohol)
summary(fit1)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 85.788430   0.732516 117.115   <2e-16 ***
#   price        0.057341   0.002611  21.960   <2e-16 ***
#   alcohol      0.046534   0.061279   0.759    0.448    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.642 on 544 degrees of freedom
# Multiple R-squared:  0.4706,	Adjusted R-squared:  0.4686 
# F-statistic: 241.8 on 2 and 544 DF,  p-value: < 2.2e-16

vif(fit1) # no collinearity
# price  alcohol 
# 1.000289 1.000289 
plot(fit1)

fit2 <- lm(log(points) ~ log(price) + alcohol)
summary(fit2)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.3758959  0.0087475 500.244   <2e-16 ***
#   log(price)  0.0282611  0.0011524  24.523   <2e-16 ***
#   alcohol     0.0008299  0.0006538   1.269    0.205    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01753 on 544 degrees of freedom
# Multiple R-squared:  0.5256,	Adjusted R-squared:  0.5239 
# F-statistic: 301.4 on 2 and 544 DF,  p-value: < 2.2e-16
vif(fit2) # no collinearity
# log(price)    alcohol 
# 1.000015   1.000015 
plot(fit2)

# b) ---------------------------------------------------------------------------


coefficients(fit2)
# (Intercept)   log(price)      alcohol 
# 4.3758959052 0.0282611175 0.0008299285  

# estimator of the variance 
sum(fit2$residuals ^2)/fit2$df.residual
# 0.0003071755


### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)

# c) ---------------------------------------------------------------------------

linearHypothesis(fit2,
                 rbind(c(0,1,0),
                       c(0,0,1)),
                 c(0,0))
# 2.2e-16, we need to reject H0, so they have an effect on the model
# d) ---------------------------------------------------------------------------
fit2 <- lm(log(points) ~ log(price) + alcohol)
summary(fit2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.3758959  0.0087475 500.244   <2e-16 ***
#   log(price)  0.0282611  0.0011524  24.523   <2e-16 ***
#   alcohol     0.0008299  0.0006538   1.269    0.205    
fit2 <- lm(log(points) ~ log(price) )
summary(fit2)

coefficients(fit2)
# (Intercept)  log(price) 
# 4.38571235  0.02825545   

# estimator of the variance 
sum(fit2$residuals ^2)/fit2$df.residual
# 0.0003075201

# e) ---------------------------------------------------------------------------

ds[,4] <- region_

lmm1 = lmer(log(points) ~ log(price) + (1|region), data = ds)
summary(lmm1)

# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE)
fixef(lmm1)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps # 0.0002433358
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b # 6.856342e-05

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.1034443


# f) ---------------------------------------------------------------------------

x11()
dotplot(ranef(lmm1))




