##########################################
# 
#       Showcase of base plot funtion
#
# Series:
# Little Useless-useful R functions #29
#
# Created: October 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com

# Changelog: 
###########################################

#load ggplot

library(magick)


#X11()     # Unix / windows

j <- 1:20
k <- j

    
png("/Users/tomazkastrun/Desktop/jkll.png")
plot(j, k, type = "l", main = "type = 'l'")
dev.off()

png("/Users/tomazkastrun/Desktop/jkss.png")
plot(j, k, type = "s", main = "type = 's'")
dev.off()

png("/Users/tomazkastrun/Desktop/jkpp.png")
plot(j, k, type = "p", main = "type = 'p'")
dev.off()

png("/Users/tomazkastrun/Desktop/jklo.png")
plot(j, k, type = "l", main = "type = 'o'")
dev.off()

png("/Users/tomazkastrun/Desktop/jkso.png")
plot(j, k, type = "s", main = "type = 'b'")
dev.off()

png("/Users/tomazkastrun/Desktop/jkph.png")
plot(j, k, type = "p", main = "type = 'h'")
dev.off()


jk_1 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jkll.png"))
jk_2 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jkss.png"))
jk_3 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jkpp.png"))
jk_4 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jklo.png"))
jk_5 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jkso.png"))
jk_6 <- image_scale(image_read("/Users/tomazkastrun/Desktop/jkph.png"))


image_resize(c(jk_1, jk_2, jk_3, jk_4, jk_5, jk_6), '400x450!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(optimize = TRUE)




