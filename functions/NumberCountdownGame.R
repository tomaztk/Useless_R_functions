##########################################
# 
# Script that finds a solution for 
# Number countdown game 
#
# Series:
# Little Useless-useful R functions #16
# Created: January 9, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

#install.packages("RcppAlgos")
library(RcppAlgos)


number_pool <- c(1:11, 25, 50, 75, 100, 200)
six <- sample(number_pool, 6, replace=FALSE)
res_num <- sample(100:999,1)
oper <- c("+","-","/","*")


d2 <- permuteGeneral(six)

for (i in 1:ceiling(length(d2)/6)){
  for (o in 1:4){
    r <- paste0(as.integer(d2[i,1]),' ',as.character(sample(oper,1)),' ',as.integer(d2[i,2]),' ',as.character(sample(oper,1)),' ', 
                as.integer(d2[i,3]),' ',as.character(sample(oper,1)),' ',as.integer(d2[i,4]),' ',as.character(sample(oper,1)),' ', 
                as.integer(d2[i,5]),' ',as.character(sample(oper,1)),' ',as.integer(d2[i,6]), sep ="")
    
    res <- eval(parse(text=r))
    if(res == res_num){
      print("solution")
      print(res)
      print(r)
      
    }
  }
}


