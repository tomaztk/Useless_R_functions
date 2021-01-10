##########################################
# 
# Script that finds a solution for 
# Number countdown game 
#
# Series:
# Little Useless-useful R functions #16
# Created: January 10, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

#install.packages("RcppAlgos")
library(RcppAlgos)


countDown_puzzle <- function(six, res_num) {
    oper <- c("+","-","/","*")
    res <- 0
    d2 <- permuteGeneral(six)
    for (i in 1:nrow(d2)){
      for (o in 1:1000){
        
        r <- paste0(as.integer(d2[i,1]),' ',as.character(sample(oper,1)),' (',as.integer(d2[i,2]),' ',as.character(sample(oper,1)),' ((', 
                    as.integer(d2[i,3]),' ',as.character(sample(oper,1)),as.integer(d2[i,4]),') ',as.character(sample(oper,1)), 
                    as.integer(d2[i,5]),') ',as.character(sample(oper,1)),as.integer(d2[i,6]), ') ', sep ="")
        #print(r)
        res <- eval(parse(text=r))
        if(res == res_num){
          print(paste0("Solution: ", r, ' with result of: ', res, ' for given the numbers: ', paste(six, collapse = " "), sep=""));
       
            }
      }
    }
}


###################################################
# run function with given six numbers and solution
##################################################

#countDown_puzzle(c(9,8,50,2,11,200), 352)
#countDown_puzzle(c(11,50,75,8,3,25), 544)
#countDown_puzzle(c(25,5,11,7,8,2), 768)
countDown_puzzle(c(100,9,10,4,1,8), 594)


#############################################
# or generate the numbers and random solution
#############################################
number_pool <- c(1:11, 25, 50, 75, 100, 200)
six <- sample(number_pool, 6, replace=FALSE)
res_num <- sample(100:999,1)

countDown_puzzle(six, res_num)
  


