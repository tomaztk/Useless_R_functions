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



# s_matrix <- function(nc,nr){
#     
#   nof <- nc*nr
#   m2 <- matrix(sample(1:100, nof, replace=TRUE), ncol=nc, nrow=nr)
#   return(m2)
# }


## keeping track of positions and steps

elements <- vector()
nr <- 5
nc <- 7
total_len <- nr*nc
m2 <- matrix(sample(1:100, total_len, replace=TRUE), ncol=nc, nrow=nr)

i = 1
row = 1
col = 1
while( length(elements) != total_len){
  #first element
  elements <- paste0(elements, m2[row, col], ";")
  # missing boundaries
  for (i in 1:nr){elements <- c(elements, m2[row, col+i])}
  for (i in 1:nr){elements <- c(elements, m2[row+i, col])}
  for (i in 1:nc){elements <- c(elements, m2[row, col-i])}
  for (i in 1:nr){elements <- c(elements, m2[row-i, col])}
}


elements
m2


matrix_spiral_unique <- function(mat) {
  mr <- nrow(mat)
  nc <- ncol(mat)
  total_len <- mr*nc
  res <- vector()
  visit <- matrix(FALSE, nrow = mr, ncol = nc)

  gor <- 1
  dol <- mr
  levo <- 1
  desno <- nc
  
  while (length(res) < total_len) {
  
    # Gor (levo)
    for (i in levo:desno) {
      if (!visit[gor, i]) {
          res <- c(res, mat[gor,i])
          visit[gor, i] <- TRUE
      }
    }
    gor <- gor + 1
    
    # Desno (dol)
    for (i in gor:dol) {
      if (!visit[i, desno]) {
          res <- c(res, mat[i,desno])
          visit[i, desno] <- TRUE
      }
    }
    desno <- desno - 1
    
    # Gor (dol)
    if (gor <= dol) {
      for (i in desno:levo) {
        if (!visit[dol, i]) {
          res <- c(res, mat[dol,i])
          visit[dol, i] <- TRUE
        }
      }
      dol <- dol - 1
    }
    
    # Levo (gor)
    if (levo <= desno) {
      for (i in dol:gor) {
        if (!visit[i, levo]) {
            res <- c(res, mat[i,levo])
            visit[i, levo] <- TRUE
        }
      }
      levo <- levo + 1
    }
  }
  return(res)
}


res <- matrix_spiral_unique(m2)

if(length(res)==length(m2)){print("Nice!")}

#Check res!
print(m2)
print(res)
