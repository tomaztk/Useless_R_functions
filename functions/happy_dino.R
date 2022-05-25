##########################################
# 
# Happy Dino graph
# 
# Series:
# Little Useless-useful R functions #38
# Created: May 25, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################

setwd("/users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")

library(ggplot2)
library(magick)
library(cowplot)

stegosaurus <- data.frame(id = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10),
                   gr = c("a","a","a","a","a","a","a","a","a","a","b","b","b","b","b","b","b","b","b","b")
                   ,v = c(1,4,10,18,30,23,15,12,2,0,0,1,10,14,35,39,28,10,8,2) )

p <- ggplot(stegosaurus, aes(x=id, y=v, fill=gr)) +
        geom_bar(stat = "identity", position="stack") +
        theme_void() +
        theme(legend.position="none")

ggdraw() +
  draw_image("happyDino_head.png",  x = -0.35, y = -0.35, scale = .4) +
  draw_plot(p)

