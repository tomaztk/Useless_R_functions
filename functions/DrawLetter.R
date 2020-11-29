##################################################
# Draw a letter :D
#
# call function draw_letter("A") and get output:
#      AA
#     AAAA
#    AA  AA
#   AA    AA
#  AAAAAAAAAA
# AA        AA
#AA          AA
##################################################


# 7 - rows - fixed
# 15 - columns - fixed
mat <- as.matrix(x=7,y=15)
#map on mat (matrix) the png raster



img <- magick::image_read("image/nikeLogo.jpg")
img <- img %>% 
  image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
  image_scale(geometry = geometry_size_pixels(width=25, height=20, preserve_aspect=FALSE)) 


# Image manipulation
mat <- t(1L - 1L * (img[[1]][1,,] > 180))
mat_df <-data.frame(mat)



# 1 - get Letter (uppercase)
# 2 - save letter as a png
# 3 - blow up the png
# 4 - make raster
# 5 - replace non empty fields with letter (uppercase)
# 6 - print


