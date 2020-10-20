##########################################
# 
# Random Data-frame maker
# Series:
# Little Useless-useful R functions #2
# Created: October 19, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################



#####################################
#####
##### Using Structure function
#####
#####################################

DataFrameMaker <- function(col,row){
  command = "dd <- structure(list( "
  for (i in 1:col){
        var = paste("v",as.character(i),"= c(",sep="")
        command = paste(command, var ,sep = "")
        for (j in 1:row){
          a <- c(i*j)
          con = paste(a,sep="")
          if ((j < row) & (j %% row != 0)){
          command = paste(command, con,",",sep = "")
          }
          else {
            command = paste(command, con,"), ",sep = "")
          }
        }
    
      }
      rn = 'row.names = c('
      for(xx in 1:row){
        rn = paste(rn, xx, 'L,', sep = "")
        if (xx == row){rn = paste(substr(rn,1,nchar(rn)-1),')', sep = "")}
      }

  command <- substr(command, 1, nchar(command)-2)
  command <- paste(command,"),", rn, "," ,"class = 'data.frame')",sep  = "")
  print(command)
  eval(parse(text=command))
}


# Run the dataframe
DataFrameMaker(4,2)




#####################################
#####
##### Using Structure function
#####
####################################

DataFrameMaker  <- function(col,row){
  dd <- matrix(nrow = row, ncol = col)
  for (i in 1:row) {
    for (j in 1:col) {
      dd[i, j] = (j*i)
    }
  }
  return(as.data.frame(dd))
  
}

# Run the dataframe
dd <- DataFrameMaker(4,2)


#####################################
#####
##### Making Use of vector datatypes
#####
####################################

DataFrameMaker <- function(i, j) {
  v1 <- rep(1:i, j)
  v2 <- rep(1:j, i)
  m1 <- matrix(v1, ncol = j, byrow = FALSE)
  m2 <- matrix(v2, ncol = j, byrow = TRUE)
  as.data.frame(m1 * m2)
}


#####################################
#####
##### Using Kronecker products on Arrays
#####
##### Contributed by Brad: https://tomaztsql.wordpress.com/2020/10/20/little-useless-useful-r-function-dataframe-maker/#comments
#####
####################################

DataFrameMaker <- function(nrow, ncol) {
  dd <- as.data.frame(kronecker(1:nrow,t(1:ncol)))
  return(dd)
}

DataFrameMaker(3,4)
