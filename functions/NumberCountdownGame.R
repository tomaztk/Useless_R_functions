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


number_pool <- c(1:11, 25, 50, 75, 100, 200)
six_nm <- sample(number_pool, 6, replace=FALSE)
ran_3digit_num <- sample(100:999,1)

oper <- c("+","*","/","-")
para <- c("(",")")
step_counter <- 0
res <- 0

# 1000 runs each ?
while (ran_3digit_num != res) {
  oper1 <- sample(oper,1,replace=TRUE)
  #2 num
  six_nm_2_1 <- sample(six_nm,1,replace=FALSE)
  six_nm_2_2 <- sample(six_nm,1,replace=FALSE)
  oper1 <- sample(oper,1,replace=TRUE)  
  
  for2 <- paste0(six_nm_2_1,oper1,six_nm_2_2)
  step_counter <- step_counter + 1
  res <- eval(parse(text=for2))

  #3 num
  six_nm_3_1 <- sample(six_nm,1,replace=FALSE)
  six_nm_3_2 <- sample(six_nm,1,replace=FALSE)
  six_nm_3_3 <- sample(six_nm,1,replace=FALSE)
  oper2_1 <- sample(oper,1,replace=TRUE)
  oper2_2 <- sample(oper,2,replace=TRUE)
  
  for3 <- paste0(six_nm_3_1,oper2_1,six_nm_3_2,oper2_2,six_nm_3_3)
  step_counter <- step_counter + 1
  res <- eval(parse(text=for3))
  
  
}

## Part 2
number_pool <- c(1:11, 25, 50, 75, 100, 200)
six <- sample(number_pool, 6, replace=FALSE)
res_num <- sample(100:999,1)

oper <- c("+","-","/","*")
para <- c("(",")")
expr <- ""

fun_itsel <- function(six, res_num, expr){
  for (op in oper){
    for (ind in seq_along(six)){
      
      number <- six[ind]  #paste0('(',six_nm[ind],op,six_nm[ind],')', collapse = NULL)
      print(res_num)
      expr <- paste0("(", number, ' ', op, ' ', expr, ")", sep = '')
      print(expr)
      
    }
  }
}

fun_itsel(six,res_num,"")