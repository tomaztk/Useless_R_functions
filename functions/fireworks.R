
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


Fireworks <- function(nof_rockets=10) {
  #rm(list=ls())
  if(!is.null(dev.list())) dev.off()
  if (!interactive()) return()

  x <-sample(1:50,nof_rockets)
  y <-sample(1:500,nof_rockets)
  bgcolor <- par("bg")
  if (bgcolor == "transparent" | bgcolor == "white") bgcolor <- "black"
  
    draw.fireworks <- function(x,y,ring) {
      par(new=TRUE)
      plot(x, y, xaxt='n', ann=FALSE, yaxt='n', frame.plot=FALSE, xlim=c(0,50),ylim=c(0,500))
      for (i in 1:ring) {   # number of rings w? different color
          color <- rainbow(ring)
          symbols(x,y, circles=0.16+i*1.2,add=T, inches=F, fg=color[i])
          #symbols(x = 33, y = 484, circles=c(0.22),add=T, inches=F, fg="blue")        
      }
    }
    
    clear.fireworks <- function(){
      #I know I need this function
      #what will this function do -> I have absolute no idea!
      a <- 2
   }

  NewYear.fireworks <- function(){  
      par(bg=bgcolor)
    
     
      for (i in 1:nof_rockets){
        ani.options(interval = 1) 
        par(new=TRUE)
        x <- x[i]
        y <- y[i]
        ring <- 10
        draw.fireworks(x,y,ring)
        clear.fireworks()
        ani.pause()
      }

  }
  
  NewYear.fireworks()
  
}


##################
# Run the function
##################

Fireworks(11)


