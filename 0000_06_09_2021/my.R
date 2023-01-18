library(fda)
setwd("C:/Users/39340/Desktop/poliMI/Applied statistics/duringTheExam/06_09_2021")
############################### EX 4 ###########################################
#
# a)
wind <- read.table("wind.txt", sep = " ", header = T)
matplot(t(wind), type = 'l', main = "Wind velocities")

abscissa <- 1:24

m <- 3
degree <- m-1
basis.1 <- create.bspline.basis(rangeval=range(abscissa),nbasis=12, norder = m)
data_W.fd.1 <- Data2fd(y = t(wind),argvals = abscissa,basisobj = basis.1)
plot.fd(data_W.fd.1)

# The first 3 coefficients of day 1
data_W.fd.1[1,]$coefs[1:3]

#
# b)  # 856.4697014 354.7200283  71.6750782  39.3710758   7.0728419   3.1243624   2.3809379
#[8]   1.3704337   0.8773049   0.7063854   0.3148967   0.2012346 
pca_W.1 <- pca.fd(data_W.fd.1,nharm=3,centerfns=TRUE)

#screen plot - only the first 12 eigenvalues are "differrent" from zero
which(pca_W.1$values > 0.01)
plot(pca_W.1$values[1:13],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:13]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

cumsum(pca_W.1$values)[1:3]/sum(pca_W.1$values)

# Plot of the first 3 Eigenfunctions
x11()
par(mfrow=c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

#
# c)

# the first two components are more than enough (min 80%)

# media <- mean.fd(data_W.fd.1)
# 
# plot(media,lwd=1,ylim=c(0,40),ylab='wind',main='FPC1')
# lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
# lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
# # more variation during the day than during the night hours
# 
# plot(media,lwd=2,ylim=c(0,40),ylab='wind',main='FPC2')
# lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
# lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
# # phase variations
# 
# plot(media,lwd=2,ylim=c(0,40),ylab='wind',main='FPC3')
# lines(media+pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=2)
# lines(media-pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=3)
# # phase variations

X11()
par(mfrow=c(1,3))
plot(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# if you look at the positive scores associated to the PC1, then they are all
# higher wrt to the mean expecially around 15, while with negative scores the 
# wind is much lower wrt to the mean during all day.

# PC2: looking at the positive scores, the wind is much higher than the mean until 15
# when it is lower wrt the mean. While the negative scores are the opposite of the positive ones.


#
# d)

media <- mean.fd(data_W.fd.1)

X11()
par(mfrow=c(1,2))

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],lwd=2)
points(pca_W.1$scores[1,1],pca_W.1$scores[1,2], col='red')
# It has a high score for both PC1 and PC2, we can say that it has very high wind velocities 
# starting from the morning but it also starts decreasing in the afternoon.
