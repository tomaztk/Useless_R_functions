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
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + geom_point() + theme_bw()


#Pipe function
ToPipe <- function(ee) {
  this_fn <- rlang::call_name(ee)
  updated_args <- rlang::call_args(ee) %>% map(ToPipe)
  
  if (identical(this_fn, "%>%") || length(updated_args)==0) {
    fn_2 <- rlang::call2("+", !!!updated_args)
    eval(fn_2)
  } else {
    arg1       <- updated_args[[1]]
    other_args <- updated_args[-1] 
    fn_3 <- rlang::call2(as.name("+"), arg1, rlang::call2(this_fn, !!!other_args)  )
   eval(fn_3)
  }
}



### pipe version
fun <- quote(ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) 
              %>% geom_point() 
               %>% theme_bw())
ToPipe(fun)


this_fn <- rlang::call_name(fun)
updated_args <- rlang::call_args(fun) 

if (identical(this_fn, "%>%") || length(updated_args)==0) {
  fn_2 <- rlang::call2("+", !!!updated_args)
  eval(fn_2)
} else {
  arg1       <- updated_args[[1]]
  other_args <- updated_args[-1] 
  fn_3 <- rlang::call2(as.name("+"), arg1, rlang::call2(this_fn, !!!other_args)  )
  eval(fn_3)
}
