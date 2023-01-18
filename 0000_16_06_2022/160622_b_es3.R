setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam - 3/0000_16_06_2022")

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


ds <- read.table('danceability.txt', header=T)
head(ds)
names(ds)
dim(ds)
any(is.na(ds))
genre_ <- factor(ds[,5])
attach(ds)

# a) ---------------------------------------------------------------------------
#Y = â0 + â1 · loudness + â2 · energy + â3 · tempo + å
fit <- lm(danceability ~ loudness + energy +tempo)
summary(fit)

coefficients(fit)
# (Intercept)    loudness      energy       tempo 
#  9.18676786  0.09930218  0.07258300 -0.00888967 

# estimator of the variance 
sum(fit$residuals ^2)/fit$df.residual
# 0.8742357
# b) ---------------------------------------------------------------------------

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)

shapiro.test(fit$residuals)
# p-value = 0.6654, it's gaussian
# The null hypothesis is states that the population is normally distributed
# i.e if the p-value is greater than 0.05, then the null hypothesis is accepted.

# plot summary lm
plot(fit)
# we can see the normality also from qqplot

# Residuals vs Fitted: is used to check the assumptions of linearity. If the residuals are spread equally around a horizontal line without distinct patterns (red line is approximately horizontal at zero), that is a good indication of having a linear relationship.
# 
# Normal Q-Q: is used to check the normality of residuals assumption. If the majority of the residuals follow the straight dashed line, then the assumption is fulfilled.
# 
# Scale-Location: is used to check the homoscedasticity of residuals (equal variance of residuals). If the residuals are spread randomly and the see a horizontal line with equally (randomly) spread points, then the assumption is fulfilled.
# 
# Residuals vs Leverage: is used to identify any influential value in our dataset. Influential values are extreme values that might influence the regression results when included or excluded from the analysis. Look for cases outside of a dashed line.

vif(fit)
# no collinearity
# c) ---------------------------------------------------------------------------

linearHypothesis(fit,
                 rbind(c(0,0,1,0),
                       c(0,1,0,0)),
                 c(0,0))
#  2.112e-06, we reject H0, they have an effect on the model


# d) ---------------------------------------------------------------------------
summary(fit)
fit <- lm(danceability ~ loudness +tempo)
summary(fit)

coefficients(fit)
# (Intercept)     loudness        tempo 
# 9.182772292  0.170266165 -0.008962479 

# estimator of the variance 
sum(fit$residuals ^2)/fit$df.residual
# 0.8791441

# e) ---------------------------------------------------------------------------
fit <- lm(danceability ~ loudness +tempo + (1|school_id))
summary(fit)

ds[,5] <- factor(ds[,5])

lmm1 = lmer(danceability ~ loudness +tempo + (1|genre), data = ds)
summary(lmm1)

# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE)
fixef(lmm1)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps # 0.7926338
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b # 0.09145383

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # 0.1034443

# f) ---------------------------------------------------------------------------

x11()
dotplot(ranef(lmm1))
# fromt he plot, it seems that the most danceable genre is R&B 

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)
head(coef(lmm1)$genre)
