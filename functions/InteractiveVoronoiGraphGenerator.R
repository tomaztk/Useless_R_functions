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
n <- 0


#### Board Graph 
voronoiGraphBoard <- function(){
if (nrow(dff)<= 2) {
  plot.new()
} else {
   ggplot(data=dff, aes(x=long,y=lat)) +
    geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
    geom_point( fill=rgb(70,130,180,255,maxColorValue=255), pch=21, size = 4,color="#333333") 
  
 
 }
}

click <- function(voronoiGraphBoard = defaultGraph){
    for (n in 1:10) {
      plot.new()
      mouse.at <- locator(n = 1, type = "o") 
      long <- rnorm(1, mouse.at$x*10,15)
      lat <- rnorm(1, mouse.at$y*10,10)
      df <- data.frame(lat,long)
      dff <<- rbind(dff, df)
      print(n)
      if (nrow(dff)>=2){
        voronoi <- deldir(dff$long, dff$lat)
        defaultGraph <<- voronoiGraphBoard()
      }
  }
}



### Start with x11 
Draw_x11 <- function(){
  #x11()
  #defaultGraph <<- voronoiGraphBoard() 
  click()
  defaultGraph
}

# Generate Voronoi
Draw_x11()


