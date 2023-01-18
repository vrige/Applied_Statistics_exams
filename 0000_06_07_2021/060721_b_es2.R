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

ds <- read.table('orthopaedics.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)
pa = 0.35
pn = 1 - pa
ab <- factor(ds[,3])

i1 <- which(ab=='AB')   # Group A: abnormal
i2 <- which(ab=='NO')   # Group B: normal

g <- 2
n1 <- length(i1)
n2 <- length(i2)
n <- n1+n2

col <- ifelse(ab=='AB','blue','red')
x11()
plot(ds[,1], ds[,2], pch=19, col=col, xlab='Inf-g', ylab='IL-5')

# a) ---------------------------------------------------------------------------

ds2 <- ds[,1:2]

# Jittering
set.seed(1)
ds2 <- ds2 + cbind(rnorm(n, sd=0.025))    # jittering

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
bartlett.test(ds2[i1,],ds[i2,])
# 6.936e-06 reject H0 (equal variance), so i am not going to apply LDA, but QDA

# One-way MANOVA (See LAB 7)
fit <- manova(as.matrix(ds2) ~ ab) # we need the same covariance
summary.manova(fit,test="Wilks")

# QDA
qda.ds <- qda(ds2, ab, prior = c(pa,pn))
qda.ds

Qda.ds <- predict(qda.ds, ds2)

table(class.true=ab, class.assigned=Qda.ds$class)

# means
qda.ds$means
# incidence     tilt
# AB  63.55852 19.58660
# NO  51.26283 12.89357

x11()
plot(ds2, main='pelvic', xlab='pelvic.incidence', ylab='pelvic.tilt_angles', pch=20)
points(ds2[i2,], col='red', pch=20)
points(ds2[i1,], col='green', pch=20)
legend("topright", legend=levels(ab), fill=c('green','red'))
points(qda.ds$means, col=c('green','red'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(ds2[i2,]), max(ds2[i2,]), length=200)
y  <- seq(min(ds2[i1,]), max(ds2[i1,]), length=200)
xy <- expand.grid(incidence=x, tilt=y)

z  <- predict(qda.ds, xy)$post  
z1 <- z[,1] - z[,2]    
z2 <- z[,2] - z[,1] 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,lty =2)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,lty =2)

# b) ---------------------------------------------------------------------------
# if we didn't have the prior we could have used:
# qda.ds <- qda(ds2, ab, CV=TRUE) 
# table(class.true=ab, class.assignedCV=qda.ds$class)
# errorsCV <- (qda.ds$class != ab)
# AERCV   <- sum(errorsCV)/length(ab)

errors_CV <- 0
for(i in 1:n){
  qda.ds <- qda(ds2[-i,], ab[-i], prior=c(pa,pn))
  errors_CV <- errors_CV + as.numeric(predict(qda.ds,ds2[i,])$class != ab[i])
}
errors_CV

AERCV   <- sum(errors_CV)/length(ab)
AERCV

# c) ---------------------------------------------------------------------------
x <- data.frame(incidence = 60, tilt = 0)
predict(qda.ds,x)$class

points(60,0, pch=3, col='springgreen', lwd=2) 

# d) ---------------------------------------------------------------------------
cost = 0.1

dat <- data.frame(x=ds2, y=as.factor(ab))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =cost, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,1))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
points(60,0, pch=3, col='black', lwd=10) 

new <- data.frame(x.incidence = 60, x.tilt = 0)
predict(svmfit, newdata = new, data=dat)
# NO