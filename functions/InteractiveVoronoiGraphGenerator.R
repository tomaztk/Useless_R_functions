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

dff <- data.frame(NULL,NULL)

#### Graph
voronoiGraphBoard <- function(){
  r <- ggplot(data=dff, aes(x=xl,y=yl)) +
    geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 1, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
    geom_point( fill=rgb(70,130,180,255,maxColorValue=255), pch=21, size = 4,color="#333333") 
  
 return(r)
}

### Clicking on canvas
click <- function(DefaultGraph=voronoiGraphBoard(), steps=st){
    DefaultGraph <- plot.new()
     #for (n in 1:10) {
      for (n in 1:steps) {
      mouse.at <- locator(n = 1, type = "o") 
      xl <- rnorm(1, mouse.at$x*10,15)
      yl <- rnorm(1, mouse.at$y*10,10)
      df <- data.frame(xl,yl)
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

# Generate Voronoi
Draw_x11(st=50)


