##########################################
# 
# Kadane's algorithm
#
# Series:
# Little Useless-useful R functions #36
# Created: March 28, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


install.packages('adagio')
library(adagio)


# single vector
maxsub(c(-2, -3, 4, -1, -2, 1, 5, -3), inds = TRUE)
  

##  Standard example: Find a maximal sum submatrix
A <- matrix(c(0,-2,-7,0, 9,2,-6,2, -4,1,-4,1, -1,8,0,2),nrow = 4, ncol = 4, byrow =TRUE)
maxsub2d(A)


#  Application to points in the unit square:
set.seed(723)
N <- 50; w <- rnorm(N)
x <- runif(N); y <- runif(N)
clr <- ifelse (w >= 0, "blue", "red")
plot(x, y, pch = 20, col = clr, xlim = c(0, 1), ylim = c(0, 1))

xs <- unique(sort(x)); ns <- length(xs)
X  <- c(0, ((xs[1:(ns-1)] + xs[2:ns])/2), 1)
ys <- unique(sort(y)); ms <- length(ys)
Y  <- c(0, ((ys[1:(ns-1)] + ys[2:ns])/2), 1)
abline(v = X, col = "gray")
abline(h = Y, col = "gray")

A <- matrix(0, N, N)
xi <- findInterval(x, X); yi <- findInterval(y, Y)
for (i in 1:N) A[yi[i], xi[i]] <- w[i]

msr <- maxsub2d(A)
rect(X[msr$inds[3]], Y[msr$inds[1]], X[msr$inds[4]+1], Y[msr$inds[2]+1])





