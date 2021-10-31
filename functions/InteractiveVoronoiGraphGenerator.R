##########################################
# 
# Interactive Voronoi graph generator 
#  with R
#
# Series:
# Little Useless-useful R functions #30
# Created: November 01, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - adding x11() / quartz
###########################################


# packages
library(deldir)
library(ggplot2)
#library(ggvoronoi)

dff <- data.frame(NULL,NULL,NULL)

#### Graph
voronoiGraphBoard <- function(){
  r <- ggplot(data=dff, aes(x=xl,y=yl)) +
    geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 1, data = voronoi$dirsgs, linetype = 1, color= "orange") +
    geom_point( shape=21, size = 3, color="red", fill="blue") +
    #geom_voronoi(aes(x=xl,y=yl,fill=distance)) +
    theme_void()
  
 return(r)
}

### Clicking on canvas
click <- function(DefaultGraph=voronoiGraphBoard(), steps=st){
    DefaultGraph <- plot.new()
     #for (n in 1:10) {
      for (n in 1:steps) {
      mouse.at <- locator(n = 1, type = "o") 
     # xl <- rnorm(1, mouse.at$x*10,15)
      xl <- mouse.at$x
      print(paste("x:", xl))
     # yl <- rnorm(1, mouse.at$y*10,10)
      yl <- mouse.at$y
      print(paste("y: ",yl))
      distance <- sqrt((xl-100)^2 + (yl-100)^2)
      df <- data.frame(xl,yl, distance)
      dff <<- rbind(dff, df)
      print(n)
      if (nrow(dff)>=2){
        voronoi <<- deldir(dff$xl, dff$yl)
        DefaultGraph <- voronoiGraphBoard()
        print(DefaultGraph)
      } else {
        print(DefaultGraph)
      }
  }
}



#### Start with x11 
Draw_x11 <- function(st){
  x11()
  click(steps=st)
  DefaultGraph <<- voronoiGraphBoard()
}

# Generate Voronoi with defined steps
Draw_x11(st=20)


