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
#        - adding distance with ggvoronoi
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
      for (n in 1:steps) {
      mouse.at <- locator(n = 1, type = "o") 
      xl <- mouse.at$x
      yl <- mouse.at$y
      distance <- sqrt((xl-100)^2 + (yl-100)^2)
      df <- data.frame(xl,yl, distance)
      dff <<- rbind(dff, df)
      if (nrow(dff)>=2){
        voronoi <<- deldir(dff$xl, dff$yl)
        DefaultGraph <- voronoiGraphBoard()
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


