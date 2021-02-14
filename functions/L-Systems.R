##########################################
# 
# L-System drawing  for ggplot2 
# Random walk
#
# Series:
# Little Useless-useful R functions #17
# Created: January , 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

# install.packages("TurtleGraphics")
library(TurtleGraphics)


fractal_tree <- function(s=100, n=2) {
  if (n <= 1) {
    turtle_forward(s)
    turtle_up()
    turtle_backward(s)
    turtle_down()
  }
  else {
    turtle_forward(s)
    a1 <- runif(1, 10, 60)
    turtle_left(a1)
    fractal_tree(s*runif(1, 0.25, 1), n-1)
    turtle_right(a1)
    a2 <- runif(1, 10, 60)
    turtle_right(a2)
    fractal_tree(s*runif(1, 0.25, 1), n-1)
    turtle_left(a2)
    turtle_up()
    turtle_backward(s)
    turtle_down()
    print(turtle_getpos())
  }
}

set.seed(123)
turtle_init(500, 600, "clip")
turtle_do({
  turtle_up()
  turtle_backward(250)
  turtle_down()
  turtle_col("blue")
  fractal_tree(100, 12)
})

