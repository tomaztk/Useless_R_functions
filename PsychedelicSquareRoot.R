##########################################
# 
# Useless and Psychedelic x11 and square
# root visualization
# Series:
# Little Useless-useful R functions #4
# Created: October 25, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################
require(RColorBrewer)

Psychedelics <- function(number_iterations)
{
  for (i in 1:number_iterations){
      x <- seq(-5*pi, 5*pi, length.out = runif(1, i*2, i*2+1))
      y <- seq(-5*pi, 5*pi, length.out = i)
      # print(x) print(y)
      r <- sqrt(outer(x^2, y^2, "+"))
      # print(r)
      image(z = exp(-r/8)*cos(r^4),  col = brewer.pal(12,"Set3"), xaxt='n', yaxt='n', ann=FALSE)
  }
}


## Run all together
## screen size can be altered
  x11(width = 15,height = 10)
  Psychedelics(105)
  dev.off()
