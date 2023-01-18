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
load("../Labs/Lab 5/mcshapiro.test.RData")

ds <- read.table('geisha.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))
summary(ds)
attach(ds)

misc <- sample(160)
ds <- ds[misc,] #permutation of the data

# a) ---------------------------------------------------------------------------
#### COMPUTING DISTANCE
###-------------------
ds_dist <- dist(ds, method='euclidean')

#### CLUSTERING
###-------------------
ds_es <- hclust(ds_dist, method='single')

#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,1))
plot(ds_es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_es, k=2)

#### CUTTING
###-------------------
cluster.es <- cutree(ds_es, k=2) # euclidean-single
clus_1 <- ds[cluster.es==1,]
clus_2 <- ds[cluster.es!=1,]

#### CENTROIDS
# centroids <- apply (geisha, 2, function (x) tapply (x, suc, mean))
M_clus_1 <- sapply(clus_1, mean)
M_clus_2 <- sapply(clus_2, mean)

# size
size_clus_1 <- dim(clus_1)[1]
size_clus_2 <- dim(clus_2)[1]

# numerosity
table(cluster.es)
  
x11()
plot(ds, col=ifelse(cluster.es==1,'red','blue'), pch=19)
plot(M_clus_1, col = 'green')#, pch = 4, cex = 2, lwd = 2)

#### COPH COEFFICIENTS
###-------------------
coph.es <- cophenetic(ds_es)

x11()
layout(rbind(c(1),c(2)))
image(as.matrix(ds_dist), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Single', asp=1 )

es <- cor(ds_dist, coph.es)
es
# If the value is high (near 1) the clustering result is an excellent 
# representation of the original distances, if it is <<1 then it is not.

# b) ---------------------------------------------------------------------------

# obviously i don't like it, because a cluster is almost empty

#### CLUSTERING
###-------------------
ds_av <- hclust(ds_dist, method='average')
ds_co <- hclust(ds_dist, method='complete')
ds_wa <- hclust(ds_dist, method='ward.D2')

#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,3))
plot(ds_av, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_av, k=2)
plot(ds_co, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_co, k=2)
plot(ds_wa, main='euclidean-ward.D2', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_wa, k=2)

# ds_wa seems the best one

#### CUTTING
###-------------------
cluster.wa <- cutree(ds_wa, k=2) # euclidean-single
clus_1 <- ds[cluster.wa==1,]
clus_2 <- ds[cluster.wa!=1,]

# centroids
# centroids <- apply (geisha, 2, function (x) tapply (x, suc, mean))
M_clus_1 <- sapply(clus_1, mean)
M_clus_2 <- sapply(clus_2, mean)

# size
size_clus_1 <- dim(clus_1)[1]
size_clus_2 <- dim(clus_2)[1]

# numerosity
table(cluster.wa)

# plot
x11()
plot(ds, col=ifelse(cluster.wa==1,'red','blue'), pch=19)

#### COPH COEFFICIENTS
###-------------------
coph.es <- cophenetic(ds_wa)

x11()
layout(rbind(c(1),c(2)))
image(as.matrix(ds_dist), main='Euclidean', asp=1 )
image(as.matrix(coph.es), main='Single', asp=1 )

es <- cor(ds_dist, coph.es)
es

# c) ---------------------------------------------------------------------------
suc <- clus_1
unsuc <- clus_2

# check gaussianity:
mcshapiro.test(suc)$pvalue
mcshapiro.test(unsuc)$pvalue

g1 <- suc
g2 <- unsuc
t1.mean <- sapply(g1,mean)
t2.mean <- sapply(g2,mean)
cov1  <-  cov(g1)
cov2  <-  cov(g2)
n1 <- dim(g1)[1]
n2 <- dim(g2)[1]
Sp    <- ((n1-1)*cov1 + (n2-1)*cov2)/(n1+n2-2)

alpha = 0.1
k  <- 4

# 4 Bonferroni intervals for the difference between suc and unsuc groups
IC_diff_means <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2),
                       t2.mean-t1.mean,
                       t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2))
IC_diff_means

# to compute the confidence intervals for the successful tour -> univariate
# MEDIA + VARIANZA for two covariates
shapiro.test(suc[,1])$p
shapiro.test(suc[,2])$p
n <- dim(suc)[1]
x.mean   <- sapply(suc,mean)
x.cov    <- sapply(suc,var)
k <- 4 
ICmean <- cbind(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
            center = x.mean,
            sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
ICmean

rbind(ICmean,IC_diff_means)

# d) ---------------------------------------------------------------------------

# we have statistically evidence that a successful tour last around 90 minutes
# and start around 16.45. Unsuccessful ones tend to last around 45 minutes and start
# around 17




