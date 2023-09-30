install.packages("mclust")
library(mclust)

data('diabetes')
head(diabetes)
str(diabetes)

install.packages("rgl")
library(rgl)

attach(diabetes)

plot3d(glucose, insulin, sspg, col="blue", size=3)

plot3d(glucose, insulin, sspg, col=rainbow(nrow(diabetes), start=0.5), size=3)

install.packages("MASS")
library("MASS")
data('geyser')
head(geyser)
str(geyser)
?geyser
attach(geyser)
summary(geyser)

#어떤 조합이 가장 많이 발생 했는지
density <- kde2d(waiting, duration, n=25)
?kde2d
open3d()
bg3d("white")
material3d(col="blue")
persp3d(density$x, density$y, density$z, col="lightblue")

#Binvariate normal
f <- function(x,y){
  mu <- c(0,0)
  rho <- 0
  sigma2 <- matrix(c(1,rho,rho,1),2,2)
  xy <- c(x,y)
  as.numeric(1/sqrt((2*pi)^2*det(sigma2))*exp(-1/2*t(xy-mu)%*%solve(sigma2)%*%(xy-mu)))
}

x <- seq(-3,3,by=0.1);y <- x
n <- length(x)
density2 <- matrix(NA,n,n)

for(i in 1:n){
  for(j in 1:n){
    density2[i,j] <- f(x[i],y[j])
  }
}

open3d()
bg3d("white")
material3d(col='blue')
persp3d(x,y,density2,col="lightblue")


data(iris)
str(iris)
attach(iris)
install.packages("rJAVA")
install.packages("iplots")
library(rJava)
library(iplots)
ibar(Species)
table(Species)
ibar(Species)
ihist(Sepal.Length)
iset()

data("quakes")
head(quakes)
