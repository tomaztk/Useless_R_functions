##########################################
# 
# Valentine useless R function Poem
#
# Series:
# Little Useless-useful R functions #17
# Created: February 02, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

library(tidyverse)


ValentinePoem <- function(){
df<- data_frame(t = seq(-pi, 0, 0.001),
           x1 = (sin(t)*sin(t)),
           x2 = x1*-1,
           #y = 15*cos(t) - 5*cos(2*t) - 3*cos(3*t) - 3*cos(4*t)
           y = sqrt(cos(t))*cos(200*t) + sqrt(abs(t)) - 0.7*(4 - t^2)^0.01
           ) %>%
gather(heart, x,x1,x2)
p <- ggplot(df, aes(x, y)) + geom_polygon(fill = "Red") + theme_void() +
  geom_text(size=5,aes(x=0, y=0, label="Roses are red, \n
      Violets are blues, \n
      I am here to write \n
      a useless heart function for you!"), col="black") +  theme(legend.position = "none")
return(p)
}

ValentinePoem()






