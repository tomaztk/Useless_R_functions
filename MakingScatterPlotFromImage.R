library(magick)
im <- magick::image_read(system.file("img", "genilogo.jpg", package="jpeg"))
im



im <- im %>% 
  image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
  image_scale(geometry = geometry_size_pixels(width=25, height=20, preserve_aspect=FALSE)) 


# Manipulating the image data to to matrix and then threshold, invert + tranpose
mat <- t(1L - 1L * (im[[1]][1,,] > 180))

mat_df <-data.frame(mat)


library(ggplot2)

# Melt data

ggplot(mat_df, 
       aes(x = x.graf 
           y = y.graf)) +
geom_point()
