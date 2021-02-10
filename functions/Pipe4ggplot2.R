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
ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + 
  geom_point()


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


pipe <- function(ee) {
  if (!rlang::is_call(ee)) { 
    return(ee) 
  }
  this_fn <- rlang::call_name(ee)
  print(this_fn)
  updated_args <- rlang::call_args(ee) %>% map(pipe)
}


pipe(ggplot())

### pipe version
pipe(ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species))) %>%  geom_point()
