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


##########

#sample DataSet
iris <- iris





ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) + 
  geom_point()
