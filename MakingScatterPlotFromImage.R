##########################################
# 
# Making scatter plot from JPG
# Series:
# Little Useless-useful R functions #9
# Created: November 19, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: - 

# Disclaimer - All functions from this series
# are written for base package; this uses also
# magick and ggplot2
###########################################

library(ggplot2)
library(magick)


setwd("/Users/tomazkastrun/Documents/GitHub/Useless_R_functions/")


img <- magick::image_read("image/amazonLogo.jpg")
img <- img %>% 
  image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
  image_scale(geometry = geometry_size_pixels(width=25, height=20, preserve_aspect=FALSE)) 


# Image manipulation
mat <- t(1L - 1L * (img[[1]][1,,] > 180))
mat_df <-data.frame(mat)



# Melt data
dff <- data.frame(x = NULL, y = NULL)
for (i in 1:nrow(mat_df)) {
  for (j in 1:ncol(mat_df)){
    if (mat_df[i,j] == 1){
      d <- data.frame(x=i, y=j)
      dff <<- rbind(dff, d)
    }
  }
}

# draw scatter
g <- ggplot(dff, aes(x = x, y = y)) + geom_point()  + scale_x_reverse() +  coord_flip()
g + theme(panel.background = element_rect(fill = "white", colour = "grey")) 

#draw scatter with jitter
g <- ggplot(dff, aes(x = x, y = y)) + geom_point() + geom_jitter() + scale_x_reverse() +  coord_flip() 
g + theme(panel.background = element_rect(fill = "white", colour = "grey"))

# draw scatter with smooth and CI
g <- ggplot(dff, aes(x = x, y = y)) + geom_point()  + scale_x_reverse() +  coord_flip() +  geom_smooth() 
g + theme(panel.background = element_rect(fill = "white", colour = "grey")) 


#Cleanup
rm(d,dff,g,mat,mat_df,i,j,im)
