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


ds <- read.table('musicCountry.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
country <- factor(ds[,3])

i1 <- which(country=='US')   # Group A: US
i2 <- which(country!='US')   # Group B: germany

g <- 2
n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

col <- ifelse(country=='US','blue','red')
x11()
plot(ds[,1], ds[,2], pch=19, col=col, xlab='Inf-g', ylab='IL-5')
# a) ---------------------------------------------------------------------------
ds2 <- ds[,1:2]

### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
for(i in 1:g)
  Ps <- c(Ps, mcshapiro.test(ds2[get(paste('i',i, sep='')),])$p) 
Ps

# 2) same covariance structure (= same covariance matrix Sigma)
S  <-  cov(ds2)
S1 <-  cov(ds2[i1,])
S2 <-  cov(ds2[i2,])
# Qualitatively:
round(S1,digits=1)
round(S2,digits=1)

x11(width=21)
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
bartlett.test(ds2[i1,],ds[i2,]) # if they are both normal, we can use this test
# 6.936e-06, so we reject H0(equal variance) and we proceed with QDA 
# however i did i mistake and i continued with LDA

fit <- manova(as.matrix(ds2) ~ country) # we need the same covariance
summary.manova(fit,test="Wilks")
# 2.2e-16 country has an effect

p_rel = 0.9

lda.ds2 <- lda(ds2, country, prior=c(1-p_rel,p_rel))
lda.ds2

lda.ds2$means
# price average.length
# Germany 59.82982       5.891951
# US      29.74888       4.377556
lda.ds2$scaling

Lda.ds <- predict(lda.ds2, ds2)

i1_assign <- which(Lda.ds$class == "US")
i2_assign <- which(Lda.ds$class != "US")


# plots of true and assigned classes
col1 <- ifelse(country=='US','blue','red')
col2 <- ifelse(Lda.ds$class=='US','blue','red')
x11()
par(mfrow=c(1,2))
plot(ds[,1], ds[,2], pch=19, col=col1, main="true labels")
plot(ds[,1], ds[,2], pch=19, col=col2, main="assigned labels")


# complete plots of true and assigned classes with contours (usually not needed)
col1 <- ifelse(country=='US','blue','red')
col2 <- ifelse(Lda.ds$class=='US','blue','red')
x11()
par(mfrow=c(1,2))
plot(ds[,1], ds[,2], pch=19, col=col1, main="true labels")
plot(ds[,1], ds[,2], pch=19, col=col2, main="assigned labels")

x  <- seq(min(ds2[i2,]), max(ds2[i2,]), length=200)
y  <- seq(min(ds2[i1,]), max(ds2[i1,]), length=200)
xy <- expand.grid(price=x, average.length=y)

z  <- predict(lda.ds2, xy)$post  
z1 <- z[,1] - z[,2]    
z2 <- z[,2] - z[,1] 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,lty =2)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,lty =2)

# b) ---------------------------------------------------------------------------

# estimate of AER(actual error rate) by cross-validation
errors_CV <- 0
for(i in 1:n){
  lda.ds <- lda(ds2[-i,], country[-i], prior=c(1-p_rel,p_rel))
  errors_CV <- errors_CV + as.numeric(predict(lda.ds,ds2[i,])$class != country[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(country)
AERCV
# 0.07978723

table(class.true=country, class.assigned=Lda.ds$class) 
#             class.assigned
# class.true Germany   US
# Germany      23      13
# US            1     151


# c) ---------------------------------------------------------------------------

# the best we can do is to use the posterior
colMeans(Lda.ds$posterior)

# d) ---------------------------------------------------------------------------

x <- data.frame(price = 50, average.length = 3.5)
predict(lda.ds2,x)$class
# US

# e) ---------------------------------------------------------------------------

dat <- data.frame(x=ds2, y=as.factor(country))
names(dat)
tune.out <- tune(svm,y~.,data=dat ,kernel = 'linear',
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10,100) ))
summary(tune.out)
# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)
# cost 10
cost <- 10
svmfit <- svm(y~., data=dat , kernel ='linear', cost =cost, scale =FALSE )
summary(svmfit)
plot(bestmod , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

new <- data.frame(x.price = 50, x.average.length = 3.5)
predict(svmfit, newdata = new, data=dat)
# US