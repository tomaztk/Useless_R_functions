##########################################
# 
# Animating dataset with simple gganimate
# 
#
# Series:
# Little Useless-useful R functions #36
# Created: April 29, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################

library(ggplot2)
library(gganimate)
library(ggthemes)


ggplot(iris, aes(factor(Species), Sepal.Length, colour = Species)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = 'Petal width: {as.numeric(format(round(frame_time, 2), nsmall = 2))}', x= 'Iris species', y =  'Sepal Length') +
   # transition_time(as.numeric(Petal.Width)) +
  transition_time(as.numeric(Petal.Width)) +
  ease_aes('sine-in-out') +
  enter_fade() + 
  theme_hc()


