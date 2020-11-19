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
library(reshape2)

# Melt data

dff <- data.frame(x = NULL, y = NULL)
for (i in 1:nrow(mat_df)) {
  for (j in 1:ncol(mat_df)){
    #print(mat_df[i,j])
    if (mat_df[i,j] == 1){
      # get position of row and column
     # print(i)
     # print(j)
      d <- data.frame(x=i, y=j)
      print(d)
      dff <- rbind(dff, d)
    }
  }
}



ggplot(dff, aes(x = x, y = y)) + geom_point()
