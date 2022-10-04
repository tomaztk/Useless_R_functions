###############
##
### Functions
###
###
### Stored as file Stats_fun.R 
### Envoke functions in any other file
### as library(Stats_fun)
###############

library(tidyverse)

groupsum <- function(df, group_vars, sum_vars){
  df %>% 
    group_by_at(vars(one_of(group_vars))) %>% 
    summarise_at(vars(one_of(sum_vars)), list(sum = sum, mean = mean))
}

# Usage:
#  groupsum(mtcars,  
#    c("carb", "vs"), 
#    c("cyl", "hp")) 
#
#  groupsum(mtcars,"cyl", "hp") 


sum_var <- function(df, var){
  summarise_at(df, vars(one_of(var)), list(sum = sum, mean = mean))
  #summarise_at(df, vars(one_of(var)), sum)
  
}


# Usage:
#  sum_var(mtcars, "cyl")
#
#  sum_var(mtcars,c("cyl", "hp"))


Spread <- function(x){
  max(x) - min(x) 
}


# Usage:
#  Spread(mtcars$cyl)
#