
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
library(forcats)
library(hrbrthemes)
library(viridis)
library(grid)
library(purrr)


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

df <- dff %>%
  mutate(x = fct_rev(fct_reorder(x,y))) %>%
  purrr::map_df(rev)

df2 <- dff %>%  mutate(x = fct_rev(fct_reorder(x,y)))
df3 <- data.frame(x = df2$x, y = df$y)
df3$x <- as.character(df3$x)


vp <- df3 %>%
  ggplot( aes(x=x, y=y, fill=x, color=y)) +
  geom_violin(width=1.5, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #theme(legend.position = "none") +
  theme_void() 

print(vp, vp=viewport(angle=-90))



### Check version with imager

# install.packages(c("cowplot","imager","tidyverse"), dependencies = TRUE)
library(tidyverse)
library(imager)
library(cowplot) 
im <- load.image("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/image/appleLogo.jpg") 
plot(im)
asc <- gtools::chr(38:126) #We use a subset of ASCII, R doesn't render the rest
head(asc,10)
txt <- imfill(50,50,val=1) %>% implot(text(20,20,"Blah")) 
txt
plot(txt,interp=FALSE)
g.chr <- function(chr) implot(imfill(50,50,val=1),text(25,25,chr,cex=5)) %>% grayscale %>% mean
g <- map_dbl(asc,g.chr)
n <- length(g)
plot(1:n,sort(g),type="n",xlab="Order",ylab="Lightness")
text(1:n,sort(g),asc[order(g)])
char <- asc[order(g)]
d <- grayscale(im) %>% imresize(.1)  %>% as.data.frame
d <- mutate(d,qv=cut_number(value,n) %>% as.integer)
d <- mutate(d,char=char[qv])
ggplot(d,aes(x,y))+geom_text(aes(label=char),size=1)+scale_y_reverse()

