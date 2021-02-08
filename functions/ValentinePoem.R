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


############################
############################
## Adding animate function
## submitted as comment
## to blog: https://tomaztsql.wordpress.com/2021/02/08/little-useless-useful-r-functions-useless-r-poem-for-valentine/
## by Jeff Monroe: https://monroeanalytics.com/
## on 08.FEB. 2021
############################
############################


library(ggplot2)
library(gganimate)
library(gifski)
library(data.table)

gen_heart_y = function(x, a) {
  (x^2)^(1 / 3) + 0.9 * (3.3 - x^2)^(1 / 2) * sin(a * pi * x)
}

heart_dt_list = lapply(seq(1, 25, by = 0.1), function(a) {
  heart_dt = data.table(x = seq(-1.8, 1.8, length.out = 500), a = a)
  heart_dt[, y := gen_heart_y(x, a)]
  return(heart_dt)
})

full_heart_dt = rbindlist(heart_dt_list)

animated_ip_heart = ggplot(full_heart_dt, aes(x, y)) +
  geom_line(color='red') +
  annotate('text', label = 'Errors are red', x = 0, y = 1, size = 8, colour = 'black') +
  annotate('text', label = 'Reserved words are blue', x = 0, y = 0.8, size = 8, colour = 'black') +
  annotate('text', label = 'Here is this useless', x = 0, y = 0.6, size = 8, colour = 'black') +
  annotate('text', label = 'R function for you', x = 0, y = 0.4, size = 8, colour = 'black') +
  theme_void() +
  transition_manual(a)

animation = animate(animated_ip_heart, width = 400, height = 400)


animation




