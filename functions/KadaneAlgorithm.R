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


## Home edition
v <- c(-3,-8, 1, -2, 1, 5, -3, -4, 3, 10, -2, 4, 1)


kadane <- function(v){
  
  max_so_far = -999999999999999 #some obnoxiously big number
  max_ending_here = 0
  start = 0
  end = 0
  s = 0
  
  for (i in 1:length(v)) {
    max_ending_here = max_ending_here  + v[i]
    if (max_so_far < max_ending_here ){
      max_so_far = max_ending_here
      start = s
      end = i
    }
    if (max_ending_here < 0) {
      max_ending_here = 0
      s = i+1
    }
  }
  #return (max_so_far)
  cat("Sum is: ", max_so_far, " with starting position: ", start, " and ending: ", end)
}


kadane(v)



#########################
#using adagio R package!
########################

#install.packages('adagio')
library(adagio)


# single vector
maxsub(v,  inds = TRUE)



#  Standard example: Find a maximal sum submatrix
A <- matrix(c(3,-2,-7,4, 9,2,-6,1, -10,2,-4,1, -5,7,2,-2),nrow = 4, ncol = 4, byrow =TRUE)

maxsub2d(A)



#  Application to points in the unit square:
set.seed(723)

N <- 50; 
w <- rnorm(N)
x <- runif(N); 
y <- runif(N)
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





