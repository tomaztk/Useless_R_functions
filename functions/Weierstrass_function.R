##########################################
#
# Weierstrass function
#
# 0 < a < 1
# b odd positive integer
# ab > 1 + 3/2 pi
#
# Series:
# Little Useless-useful R functions #53
# Created: September 03, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################


weierstrass_curve <- function(x,a,b) {
  values <- 0
  for (n in 0:100) {  
    values <- values + (a**n * cos(b**n * pi * x)) }
 return(values)
}

len <- 1000

# x <- seq(-0.4, 0.4, length.out=len)  
# y <- weierstrass_curve(x,0.5,5)
x <- seq(-2, 2, length.out=len)
y <- weierstrass_curve(x,0.3,5)

plot(x, y, type = "l", col = "red", main = "Weierstrass curve")


