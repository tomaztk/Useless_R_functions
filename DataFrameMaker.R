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



