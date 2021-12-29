
##########################################
# 
# Interactive fireworks 
#  with R
#
# Series:
# Little Useless-useful R functions #31
# Created: December 29, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - 
###########################################

library(animation)
library(ggplot2)


set.seed(2908)


Fireworks <- function(n = 10) {
  if (!interactive()) return()
  x <-sample(1:50,10)
  y <-sample(1:500,10)
  color <- rainbow(n)
  
  bgcolor <- par("bg")
  if (bgcolor == "transparent") bgcolor <- "black"
  
    draw.fireworks <- function() {
      for (i in 1:15) {   # število rink
          plot(x, y, xaxt='n', ann=FALSE, yaxt='n', frame.plot=FALSE, xlim=c(0,50),ylim=c(0,500))
          symbols(x=c(33,44,22), y=c(484,415,68), circles=c(0.16, 0.4, 0.25),add=T, inches=F, fg=c("red", "green"))
          symbols(x = 33, y = 484, circles=c(0.22),add=T, inches=F, fg="blue")        
      }
    }
    
    clear.fireworks <- function(){
      a <- 2
      #no idea what to do here :)
    }

    ani.options(interval = 1)
    
    for (i in length(x)){
      draw.fireworks()
      clear.fireworks()
    }
    ani.pause()
}



Fireworks(5)
