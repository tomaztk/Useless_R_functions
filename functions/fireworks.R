
##########################################
# 
# Tiny  fireworks  with R for New Year's 2022
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
  if(!is.null(dev.list())) dev.off()
  if(!interactive()) return()

    draw.fireworks <- function(x,y,ring) {
      plot(x, y, xaxt='n', ann=FALSE, yaxt='n', frame.plot=FALSE, xlim=c(0,50),ylim=c(0,500))
      for (i in 1:ring) {   
          ani.options(interval = 0.25) 
          color <- sample(rainbow(ring),8, replace=TRUE)
          symbols(x,y, circles=0.16+i*1.2,add=T, inches=F, fg=color[i])
          ani.pause()
      }
      par(new=TRUE)
    }
    
    clear.fireworks <- function(){
      #I know I need this function
      #what will this function do -> I have absolute no idea!
      a <- "Let's think about cleaning?!"
   }

  NewYear.fireworks <- function(){  
      bgcolor <- par("bg")
      if (bgcolor == "transparent" | bgcolor == "white") bgcolor <- "black"
      par(bg=bgcolor)
    
      xx <-sample(1:50,nof_rockets)
      yy <-sample(1:500,nof_rockets)
    
      for (i in 1:nof_rockets){
    
        x <- xx[i]
        y <- yy[i]
        ring <- ceiling(runif(1, 7, 13))
        draw.fireworks(x,y,ring)
        clear.fireworks()
      }
  }
  NewYear.fireworks()
}


##################
# Run the function
##################

Fireworks(15)


