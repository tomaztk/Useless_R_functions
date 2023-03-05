##########################################
# 
# Creating a "signature" like graph with
# xspline
#
# Series:
# Little Useless-useful R functions #50
# Created: February 01, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################

inscrp <- function(rep){
          x <- rnorm(rep)
          y <- rnorm(rep)
          plot(x,y, pch = 1, col = "white",  xaxt='n',  yaxt='n', ann=FALSE, frame.plot=FALSE)
          xspline(x,y, 1, draw = TRUE, col="blue")
}
##### run
par(mfrow = c(2,1))

inscrp(10)
inscrp(20)

par(mfrow = c(1,1))
