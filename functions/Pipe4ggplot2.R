##########################################
# 
# Pipe - a nested function for chain of 
# piped ggplot2 function calls
#
# Series:
# Little Useless-useful R functions #19
# Created: February 10, 2021
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


#more complex with function
plot_points3 <- function(.data, x, y) {
  x <- enquo(x)
  y <- enquo(y)
  averages <- .data %>%
    group_by(!!x) %>% 
    summarise(avg_y = mean(!!y, na.rm = TRUE))
  
  ggplot() +
    geom_point(data = .data, aes(!!x, !!y)) +
    geom_point(data = averages, aes(!!x, avg_y), color = "red")
}

plot_points3(iris, Species, Sepal.Length)
########################################


ToPipe <- function(ee) {
  if (!is_call(ee)) { 
    return(ee) 
  }
#  fn <- quote(ee)
#  updated_fn <- gsub("%>%", "+", quote(fn))

  this_fn <- rlang::call_name(ee)
  updated_args <- rlang::call_args(ee)
  
  if (identical(fn, "%>%") || length(updated_args)==0) {
    fn_2 <- rlang::call2("+", !!!updated_args)
    eval(parse(text = fn_2))
  } else {
    #arg1 <- updated_args[[1]]
    #call2(as.name("+"), arg1, call2(this_fn, !!!other_args) )
    print (" ")
  }
}


### pipe version
### Check working
fun <- as.quote(gplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) %>% geom_point())
ToPipe(fun)







