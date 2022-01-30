
##########################################
# 
# Plotting photos in ggplot plots using 
# horizontal violins
#
# Series:
# Little Useless-useful R functions #33
# Created: January 29, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


## Plotting photos


# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(magick)
library(stringr)
#library(forcats)
library(hrbrthemes)
library(viridis)



setwd("C:/DataTK/git/Useless_R_functions")

img <- magick::image_read("image/appleLogo.jpg")
img <- img %>% 
        image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
        image_scale(geometry = geometry_size_pixels(width=50, height=15, preserve_aspect=FALSE))

# Image manipulation
mat <- t(1L - 1L * (img[[1]][1,,] > 180))
mat_df <-data.frame(mat)


# Transpose  data
dff <- data.frame(x = NULL, y = NULL)
for (i in 1:nrow(mat_df)) {
  for (j in 1:ncol(mat_df)){
    if (mat_df[i,j] == 1){
      d <- data.frame(x=i, y=j)
      dff <<- rbind(dff, d)
    }
  }
}

# Creating factors
dff$x <- as.character(dff$x)
dff$x <- str_pad(dff$x, 3, pad = "0")


# Violine Boxplots
violinPlot <- dff %>%
          #mutate(x = fct_rev(fct_reorder(x,y))) %>% # Reorder data
          mutate(x = fct_rev(fct_reorder(x,y))) %>% 
          ggplot( aes(x=dff$x, y=dff$y, fill=dff$x, color=dff$x)) +
          geom_violin(width=1.5, size=0.2) +
         	
          scale_y_reverse() +  
          coord_flip() +  #scale_y_reverse() +
          xlab("") +
          ylab("")

violinPlot




### Part2

##########################################
# 
# Plotting photos in ggplot plots using 
# horizontal violins
#
# Series:
# Little Useless-useful R functions #33
# Created: January 29, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


## Plotting photos

#install.packages("viridis", dependencies = TRUE)
#install.packages("magick", dependencies = TRUE)
#install.packages("hrbrthemes", dependencies = TRUE)
#install.packages("forcats")

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(magick)
library(stringr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(grid)


setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions")

img <- magick::image_read("image/appleLogo.jpg")
img <- img %>% 
  image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
  image_scale(geometry = geometry_size_pixels(width=50, height=15, preserve_aspect=FALSE))

# Image manipulation
mat <- t(1L - 1L * (img[[1]][1,,] > 180))
mat_df <-data.frame(mat)


# Transpose  data
dff <- data.frame(x = NULL, y = NULL)
for (i in 1:nrow(mat_df)) {
  for (j in 1:ncol(mat_df)){
    if (mat_df[i,j] == 1){
      d <- data.frame(x=i, y=j)
      dff <<- rbind(dff, d)
    }
  }
}

# Creating factors
dff$x <- as.character(dff$x)
dff$x <- str_pad(dff$x, 3, pad = "0")


violinPlot <- dff %>%
  mutate(x = fct_rev(fct_reorder(x,y))) %>%
  purrr::map_df(rev)

violinPlot2 <- dff %>%
  mutate(x = fct_rev(fct_reorder(x,y)))

violinPlot3 <- data.frame(x = violinPlot2$x, y = violinPlot$y)
violinPlot3$x <- as.character(violinPlot3$x)


vp <- violinPlot3 %>%
  ggplot( aes(x=x, y=y, fill=y, color=y)) +
  geom_violin(width=1.5, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_void() 
  #theme_void()
#coord_flip()

#vp
print(vp, vp=viewport(angle=-90))


library(raster)
r <- raster()
r[] <- 1:ncell(r)

library(magick)
frink <- image_read("https://jeroen.github.io/images/frink.png")
obama <- image_read('https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/President_Barack_Obama.jpg/800px-President_Barack_Obama.jpg')

image_info(obama)
image_info(frink)


image_write(obama, path = '/Users/tomazkastrun/Desktop/obama.svg', format = 'svg')


image_charcoal(obama) %>% 
  image_composite(frink)  %>%
  #image_annotate("CONFIDENTIAL", size = 50, color = "red", boxcolor = "pink", degrees = 30, location = "+100+100") %>%
  image_rotate(30) %>%
  image_write('/Users/tomazkastrun/Desktop/obama_with_frink.png','png')

