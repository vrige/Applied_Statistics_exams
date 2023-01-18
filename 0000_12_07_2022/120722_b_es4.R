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


ds <- read.table('temperatures.txt', header=T)

names(ds)
dim(ds)
any(is.na(ds))
head(ds)

n = dim(ds)[1]
p = dim(ds)[2]

attach(ds)

ds$year <- factor(year) #factor(ifelse(year==2022,1,0))#
ds$park <- factor(park)

coordinates(ds) <- c('x','y') 

# year anisotopy
# no anis
plot(variogram(temperature ~ year, ds,  cutoff = 7000),pch=19) 
# y anis
plot(variogram(temperature ~ year, ds, alpha = c(0, 45, 90, 135)),pch=19)
# I assumed isotropy is a bit of a stretch, but I can assume it more or less. 
# Also the variogram doesn't seem to stabilize well so I try to extend the
# cutoff and see if it stabilizes further

# park anisotopy
# no anis
plot(variogram(temperature ~  park,ds),pch=19) 
# y anis
plot(variogram(temperature ~ park, ds, alpha = c(0, 45, 90, 135)),pch=19)
# Isotropy is assumed, and the variogram stabilises well since the beginning

# a) ---------------------------------------------------------------------------

formula <- formula(temperature  ~ year)

v.no <- variogram(formula, data=ds)
plot(v.no,pch=19)
v.fit <- fit.variogram(v.no, vgm(0.1, "Sph", 3500))    ###vgm(sill, 'type', range, nugget)
plot(v.no, v.fit, pch = 3)
v.fit
#     model      psill    range
# 1   Sph   0.07875956 2145.324

g <- gstat(formula = formula, data = ds, model = v.fit)
g$model

# this gives the estimate of the mean (drift component) under gls:
# 2003
a0 <- predict(g, ds[1,], BLUE = TRUE)
a0
#           coordinates var1.pred    var1.var
# 1 (512017.7, 5030066)  35.66398 0.003136061

# 2022
a1 <- predict(g, ds[128,], BLUE = TRUE)
a1
#         coordinates     var1.pred    var1.var
# 128 (513976.9, 5037250)  30.98347 0.003123861

# The parameters estimated via GLS are:
# 2003---35.66398 and 2022---30.98347

# The universal Kriging model assumes z_s = m_s + delta_s for any s in D (domain) where m_s is
# called drift and describes the non constant spacial mean variation. Moreover we assume
# E[delta_s] = 0 for any s in D (so that E[z_s] = m_s) and that Cov(z_s1,z_s2) =
#   Cov(delta_s1,delta_s2) for any pair. 

# notice that The two main assumptions for kriging to provide best linear unbiased 
# prediction are those of stationarity and isotropy,
# Ordinary kriging, for which the assumption of stationarity (that the mean and
# variance of the values is constant across the spatial field) must be assumed. 
# Universal kriging, which relaxes the assumption of stationarity by allowing the
# mean of the values to differ in a deterministic way in different locations 
# (e.g. through some kind of spatial trend), while only the variance is held 
# constant across the entire field.

# b) ---------------------------------------------------------------------------


formula2 <- formula(temperature  ~ park)

v.no2 <- variogram(formula2, data=ds)
plot(v.no2,pch=19)
v.fit2 <- fit.variogram(v.no2, vgm(5.4, "Sph", 1000))    
plot(v.no2, v.fit2, pch = 3)
v.fit2
#    model    psill    range
# 1    Sph 5.675135 305.2353

g2 <- gstat(formula = formula2, data = ds, model = v.fit2)


# this gives the estimate of the mean (drift component) under gls:
# 2003
a0 <- predict(g2, ds[1,], BLUE = TRUE)
a0
#           coordinates var1.pred   var1.var
# 1 (512017.7, 5030066)  33.38337 0.05390913

# 2022
a1 <- predict(g2, ds[3,], BLUE = TRUE)
a1
#         coordinates var1.pred  var1.var
# 3 (512181, 5035454)  33.21578 0.3081224

# The parameters estimated via GLS are:
# 1---33.38337 and 0---33.21578

# The universal Kriging model assumes z_s = m_s + delta_s for any s in D (domain) where m_s is
# called drift and describes the non constant spacial mean variation. Moreover we assume
# E[delta_s] = 0 for any s in D (so that E[z_s] = m_s) and that Cov(z_s1,z_s2) =
#   Cov(delta_s1,delta_s2) for any pair. 

# c) ---------------------------------------------------------------------------

# I am using universal kriging assumptions.
# Clearly the model at point A is more appropriate to fit the data: although the variogram
# stabilizes in both and isotropy can be assumed in both, the second model doesn't fit a spherical
# model without nugget, since it stabilizes immediately and with a high psill. It would fit more
# appropriately a pure nugget type of model, which in practice means no spatial correlation, and
# stationarity.
# Moreover, it makes sense to distinguish temperatures recorded in different years, since global
# temperatures have probably been increasing everywhere: using only park mixes temperature
# recorded in very different timeframes, which are probably not so much as spatially correlated
# between the years.

# d) ---------------------------------------------------------------------------

s0.new <- data.frame(x=513852.78,y=5035411.95,year="2022",park="1")
coordinates(s0.new) <- c('x','y')
pr<- predict(g, s0.new)
pr
#         coordinates var1.pred   var1.var
# 1 (513852.8, 5035412)  31.24886 0.02190528



