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
 


cols <- colorRampPalette(c("blue","yellow","red","black"))(11)

# Parameters
xmin <- -2
xmax <- 1
nx <- 250
ymin <- -1.5
ymax <- 1.5
ny <- 250
n <- 400


# variables
x <- seq(xmin, xmax, length.out=nx)
y <- seq(ymin, ymax, length.out=ny)
c <- outer(x,y*1i,FUN="+")
z <- matrix(0.0, nrow=length(x), ncol=length(y))
k <- matrix(0.0, nrow=length(x), ncol=length(y))



for (rep in 1:n) { 
  print(rep)
  for (i in 1:nx) { 
    for (j in 1:ny) { 
      if(Mod(z[i,j]) < 2 && k[i,j] < n) {
        z[i,j] <- z[i,j]^2 + c[i,j]
        k[i,j] <- k[i,j] + 1
      }
    }
  }
}

image(x,y,k, col=cols)



