
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
#        - add clean rings?
###########################################

library(animation)
library(ggplot2)


set.seed(2908)


Fireworks <- function(nof_rockets=10) {
  if(!is.null(dev.list())) dev.off()
  if(!interactive()) return()

    draw.fireworks <- function(x,y,ring) {
      plot(x, y, xaxt='n', ann=FALSE, yaxt='n', frame.plot=FALSE, xlim=c(0,50),ylim=c(0,500))
      title(main = "Happy New Year 2022", col.main= "white")
      for (i in 1:ring) {   
          ani.options(interval = 0.25) 
          color <- sample(rainbow(ring),8, replace=TRUE)
          symbols(x,y, circles=0.16+i*1.2,add=T, inches=F, fg=color[i])
          ani.pause()
      }
      par(new=TRUE)
    }
    
    clear.fireworks <- function(x,y,ring){
      #
      plot(x, y, xaxt='n', ann=FALSE, yaxt='n', frame.plot=FALSE, xlim=c(0,50),ylim=c(0,500))
      for (i in 1:ring) {   
        ani.options(interval = 0.15) 
        symbols(x,y, circles=0.16+i*1.2,add=T, inches=F, fg="black")
        ani.pause()
      }
      par(new=TRUE)
   }

  NewYear.fireworks <- function(){  
      bgcolor <- par("bg")
      if (bgcolor == "transparent" | bgcolor == "white") bgcolor <- "black"
      par(bg=bgcolor)
    
     # nof_rockets <- 10
      xx <-sample(1:50,nof_rockets)
      yy <-sample(1:500,nof_rockets)
      ringy <- sample(7:13,nof_rockets, replace = TRUE)
      
      for (i in 1:nof_rockets){
    
        x <- xx[i]
        y <- yy[i]
        ring <- ringy[i]
        draw.fireworks(x,y,ring)
        # if you don't want rings disapearing, comment this IF statement
        if (i > 1)  {
          x1 <- xx[i-1]
          y1 <- yy[i-1]
          ring1 <- ringy[i-1]
          clear.fireworks(x1, y1, ring1)
          }
      }
      # if you don't want rings disapearing, comment this IF statement
      clear.fireworks(tail(xx,1), tail(yy,1), tail(ringy,1))
  }
  NewYear.fireworks()

}


##################
# Run the function
##################

Fireworks(10)

  

