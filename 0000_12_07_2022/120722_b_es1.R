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


ds <- read.table('dnd_monsters.txt', header=T)
head(ds)
names(ds)
dim(ds)
any(is.na(ds))

n = dim(ds)[1]
p = dim(ds)[2]

size_ <- factor(ds[,9])
ds <- ds[,1:8]
# exploration
x11()
boxplot(ds,col='gold')
# we can already observe that the variability of hit points
# is much larger than the rest of the covariates

x11()
boxplot(scale(x=ds, center = T, scale=F), col='gold')

S <- cov(ds)
round(S,digits = 2)
R <- cor(ds)
round(R,digits = 2)

# a) ---------------------------------------------------------------------------
##### based on the correlation matrix
ds.sd <- scale(ds)
ds.sd <- data.frame(ds.sd)

x11()
boxplot(ds.sd,col='gold')

head(ds.sd)
m <- sapply(ds.sd,mean)
std <- sapply(ds.sd,sd)
cov(ds.sd)

# PCA on the correlation matrix
pc.ds <- princomp(ds.sd, scores=T)
summary(pc.ds)
# the first three components explain the 86% of the variaiblity

# b) ---------------------------------------------------------------------------

plot(pc.ds$loadings[,1],pc.ds$loadings[,2])

load.ds <- pc.ds$loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3)barplot(load.ds[,i], ylim = c(-1, 1))

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(ifelse(abs(load.ds[,i]) < 0.3, 0, load.ds[,i]) , ylim = c(-1, 1));abline(h=0)


# we can say that the first loading corresponds to the average of all the attributes
# excpet dexterity, while the second one corresponds to dexterity and wisdomin constrast
# with strenght and constitution
# in fact, the first plot can be describe with all positive and equal values expect dexterity),
# while it is very high in the second loading, so it is isolated in the top-left corner.

# c) ---------------------------------------------------------------------------
x11()
plot(pc.ds$scores[,1],pc.ds$scores[,2], col=size_)

legend("topright", legend=levels(size_), fill=c(1,2,3,4,5,6))

# from the plot we can observe that tiny, small and mediaum monsters tend to have more dextrity
# while large, huge and gargantuan tend to have less dexterity (because we know it was isolated in the topleft corner)

# d) ---------------------------------------------------------------------------
i1 <- which(size_ == "Tiny")
i2 <- which(size_ == "Huge")
ds2 <- rbind(ds[i1,],ds[i2,])
size_2 <- factor(c(size_[i1],size_[i2]))
cost <- 10

dat <- data.frame(ds2, y=as.factor(size_2))
names(dat)
svmfit <- svm(y~., data=dat , kernel ='linear', cost =cost, scale =FALSE )
summary(svmfit)

new <- data.frame(armor.class=14, hit.points=50, strength=19,dexterity=10,
                  constitution=16, intelligence=8, wisdom=12, charisma=13)
predict(svmfit, newdata = new, data=dat)
# Huge


