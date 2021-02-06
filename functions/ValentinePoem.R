##########################################
# 
# Valentine useless R function Poem
# in a Heart
#
# Series:
# Little Useless-useful R functions #18
# Created: February 06, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
###########################################

library(tidyverse)


ValentinePoem <- function(){
df<- data_frame(t = seq(-pi, 0, 0.001),
           x1 = (sin(t)*sin(t)),
           x2 = x1*-1,
           y = sqrt(cos(t))*cos(200*t) + sqrt(abs(t)) - 0.7*(4 - t^2)^0.01
           ) %>%
gather(heart, x,x1,x2)
p <- ggplot(df, aes(x, y)) + geom_polygon(fill = "Red") + theme_void() +
  geom_text(size=6,aes(x=0, y=0, label="Errors are red, \n
      Reserved words are blue, \n
      I am here to write a useless\n
      heart function for you!"), col="black") +  theme(legend.position = "none")
return(p)
}


# Run function
ValentinePoem()






