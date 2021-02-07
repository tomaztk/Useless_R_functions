##########################################
# 
# Valentine useless R function Poem
# in a Heart
#
# Series:
# Little Useless-useful R functions #18
# Created: February 07, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#    - sin + sin = heart
#    - cos + cos = angel wings
###########################################

library(tidyverse)


ValentinePoem <- function(){
df<- data_frame(sq = seq(-30, 0, 0.005),
           x1 = (sin(sq)*sin(sq)),
           x2 = x1*-1,
           y = sqrt(cos(sq))*cos(200*sq) + sqrt(abs(sq)) - 0.7*(4 - sq^2)^0.01
           ) %>%
gather(heart, x,x1,x2)
p <- ggplot(df, aes(x, y)) + geom_polygon(fill = "Red") + theme_void() +
  geom_text(size=6,aes(x=0, y=0, label="Errors are red, \n
      Reserved words are blue, \n
      I am here writing this useless\n
      R heart function for you!"), col="black") +  theme(legend.position = "none")
return(p)
}


# Run function
ValentinePoem()






