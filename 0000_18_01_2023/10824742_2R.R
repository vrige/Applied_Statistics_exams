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


ds <- read.table('beers.txt', header=T)
head(ds)
dim(ds)
any(is.na(ds))

attach(ds)
n <- dim(ds)[1]
p <- dim(ds)[2]
detach(ds)

# a) ---------------------------------------------------------------------------
misc <- sample(n)
ds <- ds[misc,] #permutation of the data

ds.sc <- scale(ds)

# a) ---------------------------------------------------------------------------
#### COMPUTING DISTANCE
###-------------------
ds_dist <- dist(ds.sc, method='euclidean')

#### CLUSTERING
###-------------------
ds_si <- hclust(ds_dist, method='single')
ds_co <- hclust(ds_dist, method='complete')

#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,2))
plot(ds_si, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_si, k=2)
plot(ds_co, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(ds_co, k=2)


# b) ---------------------------------------------------------------------------
#### CUTTING
###-------------------
cluster.es <- cutree(ds_co, k=2) # euclidean-complete
cluster.si <- cutree(ds_co, k=2) # euclidean-single
clus_1 <- ds[cluster.es==1,]
clus_2 <- ds[cluster.es!=1,]

i1 <- cluster.es==1
i2 <- cluster.es!=1
fact <- factor(cluster.es)
g <- 2
p <- 2

# I would not choose the Single linkage because the two groups are not correctly separated (chain effect)
# It is obvious from the plot
plot(ds, col =cluster.si )
plot(ds, col =cluster.es )
# c) ---------------------------------------------------------------------------

#### CENTROIDS
###-------------------
# centroids <- apply (geisha, 2, function (x) tapply (x, suc, mean))
M_clus_1 <- sapply(clus_1, mean)
M_clus_2 <- sapply(clus_2, mean)

M_clus_1
#   alcohol       ibu 
#   5.901857 40.059429 
M_clus_2
#   alcohol        ibu 
#   9.415273 100.624545 

#### SIZE
###-------------------
dim(clus_1)[1] #70
dim(clus_2)[1] #55


#### COPH COEFFICIENTS
###-------------------
coph.co <- cophenetic(ds_co)

x11()
layout(rbind(c(1),c(2)))
image(as.matrix(ds_dist), main='Euclidean', asp=1 )
image(as.matrix(coph.co), main='Single', asp=1 )

es <- cor(ds_dist, coph.co) # 0.7919285
es
# If the value is high (near 1) the clustering result is an excellent 
# representation of the original distances, if it is <<1 then it is not.

si <- cor(ds_dist, cophenetic(ds_si)) # 0.7677091
si
