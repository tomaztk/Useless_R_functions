##########################################
# 
# L-System drawing for Turtle Graphics
# Random walk
#
# Series:
# Little Useless-useful R functions #20
# Created: February 16, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

library(TurtleGraphics)


# common function
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

###########################################
###########################################
### Randomised L-System
###########################################
###########################################

random_turtle <- function(){
    
    f <- ""
    single_com <- function(){
      list_com <- c("turtle_left(","turtle_right(")
      angle <- sample(1:120, 1, TRUE)
      com <- sample(list_com,1,TRUE)
      return(paste0(com, angle, ")\n"))
    }
    
    comm1 <- "set.seed(2908)
    turtle_init(600, 500, 'clip')
    turtle_hide()
    i <- 8
    j <- 500
    turtle_do({"
    
    for (i in 1:10){
      sc <- single_com()
      #sc2 <- single_com2()
      i <- i + 1
      f <- paste(f, sc, collapse = NULL)
      #print(f)
      comm2 <<- f
    }
    
    comm3 <- "
    turtlebump(i,j)
    })"
    
    fin <- paste0(comm1, comm2, comm3)
    eval(parse(text=fin))

}

#run random function
random_turtle()
