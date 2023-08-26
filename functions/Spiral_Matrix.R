##########################################
#
# Spiral Matrix
#
# Given a matrix of m ✕n elements (m rows, n columns), 
# return all elements of the matrix in spiral order.
#
# Series:
# Little Useless-useful R functions #52
# Created: August 25, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################



spiral_matrix <- function(n,m){
    
  nof <- n*m
  m2 <- matrix(sample(1:100, nof, replace=TRUE), ncol=n, nrow=m)
  return(m2)
}

spiral_matrix(4,7)

