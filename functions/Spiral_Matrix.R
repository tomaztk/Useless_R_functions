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


# Helper function
make_matrix <- function(nc,nr){

  nof <- nc*nr
  mat1 <- matrix(sample(1:100, nof, replace=TRUE), ncol=nc, nrow=nr)
  return(mat1)
}


## return elements from matrix in spiral order
matrix_spiral <- function(mat) {
  mr <- dim(mat)[1]
  nc <- dim(mat)[2]
  total_len <- mr*nc

  #path #TRUE -> visited; FALSE -> unvisited
  visit <- matrix(FALSE, nrow=mr, ncol=nc)
  
  #helper variables
  gor <- 1
  dol <- mr
  levo <- 1
  desno <- nc
  res <- vector()
  
  while (length(res) < total_len) {

    for (i in levo:nc) {
      if (!visit[gor, i]) {
          res <- c(res, mat[gor,i])
          visit[gor, i] <- TRUE
      } }
    gor <- gor + 1

    for (i in gor:mr) {
      if (!visit[i, desno]) {
          res <- c(res, mat[i,desno])
          visit[i, desno] <- TRUE
      } }
    desno <- desno - 1
    
    if (gor <= dol) {
      for (i in desno:levo) {
        if (!visit[dol, i]) {
          res <- c(res, mat[dol,i])
          visit[dol, i] <- TRUE
        } }
      dol <- dol - 1
    }
    
    if (levo <= desno) {
      for (i in dol:gor) {
        if (!visit[i, levo]) {
            res <- c(res, mat[i,levo])
            visit[i, levo] <- TRUE
        } }
    levo <- levo + 1
    } }
  return(res)
}


# run functions
mat2 <- make_matrix(4,6)
res <- matrix_spiral(mat2)
#res <- matrix_spiral(make_matrix(7,5))

#Check results!
if(length(res)==length(mat2)){print("Nice, all elements are incl!")}
print(mat2)
print(res)
