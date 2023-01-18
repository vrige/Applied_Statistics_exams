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

ds <- read.table('pinnanobilis.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
attach(ds)

n = dim(ds)[1]
p = dim(ds)[2]

misc <- sample(n)
ds <- ds[misc,] #permutation of the data

# a) ---------------------------------------------------------------------------
#### COMPUTING DISTANCE
###-------------------
ds_dist <- dist(ds, method='euclidean')

#### CLUSTERING
###-------------------
ds_co <- hclust(ds_dist, method='complete')

#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,1))
plot(ds_co, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_co, k=2)

#### CUTTING
###-------------------
cluster.es <- cutree(ds_co, k=2) # euclidean-complete
clus_1 <- ds[cluster.es==1,]
clus_2 <- ds[cluster.es!=1,]

#### CENTROIDS
###-------------------
# centroids <- apply (geisha, 2, function (x) tapply (x, suc, mean))
M_clus_1 <- sapply(clus_1, mean)
M_clus_2 <- sapply(clus_2, mean)

# b) ---------------------------------------------------------------------------
i1 <- cluster.es==1
i2 <- cluster.es!=1
fact <- factor(cluster.es)
g <- 2
p <- 2

### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
for(i in 1:g)
  Ps <- c(Ps, mcshapiro.test(ds[get(paste('i',i, sep='')),1:p])$p) 
Ps

# 2) same covariance structure (= same covariance matrix Sigma)
S  <-  cov(ds)
S1 <-  cov(ds[i1,])
S2 <-  cov(ds[i2,])
# bartlett.test(ds[,1:2],cluster.es)

fit <- manova(as.matrix(ds) ~ fact)
summary.manova(fit,test="Wilks")
# fact has an effect

# which covariate is responsible?
summary.aov(fit)
# both

# Estimate of variances
W <- summary.manova(fit)$SS$Residuals/80 # 80 degrees of freedom (of residuals)
W2 <- diag(t(fit$res) %*% fit$res)/(n-g) 

m  <- sapply(ds,mean)         # estimates mu
m1 <- sapply(ds[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(ds[i2,],mean)    # estimates mu.2=mu+tau.2

# Estimate tau.i, beta.j:
tau_h   <- m[1] - m1[1]  # tau.1.1
tauC_w  <- m[2] - m1[2]  # tau.1.2

tau_2_h <- m[1] - m2[1]  # tau.2.1
tau_2_w <- m[2] - m2[2]  # tau.2.2

# c) ---------------------------------------------------------------------------

alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)
n1 <- length(i1)
n2 <- length(i2)

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
CI <- cbind(inf12,sup12)
CI
# shells from the first population are much higher and wider
# (notice that the corrisponding test was meant to check if all the means were equals)


