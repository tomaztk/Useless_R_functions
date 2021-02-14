##########################################
# 
# L-System drawing for Turtle Graphics
# Random walk
#
# Series:
# Little Useless-useful R functions #20
# Created: February 14, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

library(TurtleGraphics)


turtlebump <- function(i, j) {
  if (i==0) {
    turtle_forward(10)
  } else {
    turtlebump(i-1, j)
    turtle_left(60)
    turtlebump(i-1, j)
    turtle_right(60)
    turtle_right(60)
    turtlebump(i-1, j)
    turtle_left(60)
  }
}


set.seed(2908)
turtle_init(600, 500, "clip")
turtle_hide()
i <- 8
j <- 500
turtle_do({
    turtle_up()
    turtle_left(90)
    turtle_forward(120)
    turtle_forward(120)
  
    turtle_right(60)
    turtle_right(60)
    turtle_right(60)
    turtle_down()
    turtlebump(i,j)
  })


