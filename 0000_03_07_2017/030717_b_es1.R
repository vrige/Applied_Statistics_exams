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

kimono <- read.table('kimono.txt', header=T)
head(kimono)
attach(kimono)

city_ <-  factor(city)
type_ <-  factor(type)
dim(kimono)

c1 <- which(levels(city_)[1] == city_)
c2 <- which(levels(city_)[2] == city_)

t1 <- which(levels(type_)[1] == type_)
t2 <- which(levels(type_)[2] == type_)

any(is.na(kimono))
summary(kimono)

### Data exploration
x11()
plot(city_, value, xlab='city', ylab='value', col='grey85', main='kimono s value')

x11()
plot(type_, value, xlab='type', ylab='value', col='grey85', main='kimono s value')

g <- length(levels(city_))
b <- length(levels(type_))
n <- length(value)/(g*b)  #number of obs for each combination


kimono[,4] <- ifelse(kimono[,2]==levels(city_)[1],
                     ifelse(kimono[,3]==levels(type_)[1],'KH','KR'),
                     ifelse(kimono[,3]==levels(type_)[1],'TH','TR'))
city_type  <- factor(kimono[,4])

M          <- mean(value)
Mcity      <- tapply(value,      city_, mean)
Mtype      <- tapply(value,      type_, mean)
Mcity_type <- tapply(value, city_type , mean)

x11()
par(mfrow=c(2,3),las=2)
barplot(rep(M,4), names.arg=levels(city_type), ylim=c(0,24), main='No factor')
barplot(rep(Mcity,each=2), names.arg=levels(city_type), ylim=c(0,24), 
        col=rep(c('blue','red'),each=2), main='Only Fact. city')
barplot(rep(Mtype,times=2), names.arg=levels(city_type), ylim=c(0,24),
        col=rep(c('darkgreen','orange'),times=2), main='Only Fact. type')
barplot(c(Mcity[1]+Mtype[1]-M, Mcity[1]+Mtype[2]-M, Mcity[2]+Mtype[1]-M, 
          Mcity[2]+Mtype[2]-M), names.arg=levels(city_type), ylim=c(0,24), 
        col=rep(c('darkgreen','orange'),times=2), density=rep(10,4), angle=135, 
        main='Additive model city+type')
barplot(c(Mcity[1]+Mtype[1]-M, Mcity[1]+Mtype[2]-M, Mcity[2]+Mtype[1]-M, 
          Mcity[2]+Mtype[2]-M), names.arg=levels(city_type), ylim=c(0,24), 
        col=rep(c('blue','red'),each=2), density=rep(10,4), add=T)
barplot(Mcity_type, names.arg=levels(city_type), ylim=c(0,24), 
        col=rainbow(5)[2:5], main='Model with Interact. city+type')
plot(city_type, value, col=rainbow(5)[2:5], ylim=c(0,24),xlab='')

# a) ###########################################################################

### Two-ways ANOVA
###----------------

### Model with interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
###     i=1,2 (effect station), j=1,2 (effect gasoline)

fit.aov2.int <- aov(value ~ city + type + city:type)
summary.aov(fit.aov2.int)

# model assumptions:
# 1) normality (univariate) in each group (4 tests)
Ps <- c(shapiro.test(kimono[kimono[,4] == 'TH',1 ])$p,
        shapiro.test(kimono[kimono[,4] == 'TH',1 ])$p,
        shapiro.test(kimono[kimono[,4] == 'KR',1 ])$p,
        shapiro.test(kimono[kimono[,4] == 'KH',1 ])$p)
Ps

# 2) homogeneity of variances
bartlett.test(value, city_)
bartlett.test(value, type_)



# b) ###########################################################################


fit.aov2.int <- aov(value ~ city + type + city:type)
summary.aov(fit.aov2.int)
# Df Sum Sq Mean Sq   F value Pr(>F)    
# city          1      2       2     0.930  0.335    
# type          1  29343   29343 14528.170 <2e-16 ***
# city:type     1      0       0     0.124  0.725    
# Residuals   524   1058       2                     

fit.aov2.int <- aov(value ~ city + type)
summary.aov(fit.aov2.int)
# Df Sum Sq Mean Sq   F value Pr(>F)    
# city          1      2       2     0.932  0.335    
# type          1  29343   29343 14552.451 <2e-16 ***
#   Residuals   525   1059       2                     

fit.aov2.int <- aov(value ~ type)
summary.aov(fit.aov2.int)

### => we verify the assumptions on the reduced model (one-way ANOVA)
# 1) normality (univariate) in each group (2 tests)
Ps <- c(shapiro.test(kimono[ type_==levels(type_)[1],1 ])$p,
        shapiro.test(kimono[ type_==levels(type_)[2],1 ])$p)
Ps

# 2) homogeneity of variances
bartlett.test(value, type_)

# c) ###########################################################################

### Which type is responsible for this? To see this, we need to 
### do g*(g-1)/2 comparisons.
### We use Bonferroni
k <- g*(g-1)/2
alpha<- 0.05
n <- length(kimono[,1])
ng <- table(type_) 

Mediag  <- tapply(value, type_, mean)
SSres <- sum(residuals(fit.aov2.int)^2)
S <- SSres/(n-g)

# BOnferroni
paste(levels(type_),"-",levels(type_)[2])
as.numeric(c(Mediag[1]-Mediag[2] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] )),
             Mediag[1]-Mediag[2],
             Mediag[1]-Mediag[2] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[1] + 1/ng[2] ))))

