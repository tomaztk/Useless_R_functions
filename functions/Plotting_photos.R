
##########################################
# 
# Converting JPG and plots raster using 
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
pkg <- c("dplyr", "tidyr", "ggplot2", "magick", "stringr",
         "forcats", "viridis", "grid", "purrr","hrbrthemes")
lapply(pkg, require, character.only = TRUE)


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
dff$x <- str_pad(as.character(dff$x), 3, pad = "0")

# Reversing order
df <- dff %>%
  mutate(x = fct_rev(fct_reorder(x,y))) %>%
  purrr::map_df(rev)

df2 <- dff %>% mutate(x = fct_rev(fct_reorder(x,y)))
df3 <- data.frame(x = as.character(df2$x), y = df$y)


vp <- df3 %>%
  ggplot( aes(x=x, y=y, fill=x, color=y)) +
  geom_violin(width=1.5, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_void() 

print(vp, vp=viewport(angle=-90))
