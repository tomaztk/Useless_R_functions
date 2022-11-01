##########################################

#
# The Mandelbrot set
#
#
# Series:
# Little Useless-useful R functions #43
# Created: October 15, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################

cols <- colorRampPalette(c("white","black","white","grey","black"))(11)
n <- 400


# variables
x <- seq(-2, 1, length.out=250)
y <- seq(-1.5, 1.5, length.out=250)
c <- outer(x,y*1i,"+")
z <- matrix(0.0, nrow=length(x), ncol=length(y))
k <- matrix(0.0, nrow=length(x), ncol=length(y))


for (rep in 1:n) { 
  for (i in 1:250) { 
    for (j in 1:250) { 
      if(Mod(z[i,j]) < 2 && k[i,j] < n) {
        z[i,j] <- z[i,j]^2 + c[i,j]
        k[i,j] <- k[i,j] + 1
      }
    }
  }
}

image(x,y,k, col=cols, axes = FALSE, xlab = "" , ylab = "" )






