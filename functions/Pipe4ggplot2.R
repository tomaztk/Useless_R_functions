##########################################
# 
# Pipe - a nested function for chain of 
# piped ggplot2 function calls
#
# Series:
# Little Useless-useful R functions #19
# Created: February 12, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

library(ggplot2)
library(dplyr)
library(rlang)

#sample DataSet
iris <- iris
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + geom_point()


#Pipe function
ToPipe <- function(ee) {
  this_fn <- rlang::call_name(ee)
  updated_args <- rlang::call_args(ee)
  
  if (identical(this_fn, "%>%") || length(updated_args)==0) {
    fn_2 <- rlang::call2("+", !!!updated_args)
    eval(fn_2)
  } else {
   eval(ee)
  }
}



### pipe version
fun <- quote(ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) 
              %>% geom_point())
ToPipe(fun)