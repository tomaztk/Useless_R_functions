
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
          scale_fill_viridis(discrete=TRUE) +
          scale_color_viridis(discrete=TRUE) +
          theme_void(legend.title=element_blank()) +
          scale_y_reverse() +  
          coord_flip() +  #scale_y_reverse() +
          xlab("") +
          ylab("")

violinPlot

