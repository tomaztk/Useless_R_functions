# Draw a letter :D
##################################################
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

img <- magick::image_read("/Users/tomazkastrun/Documents/GitHub/Useless_R_functions/image/nikeLogo.jpg")
img <- img %>% 
  image_quantize(max=2, colorspace = 'gray', dither=TRUE) %>%
  image_scale(geometry = geometry_size_pixels(width=25, height=20, preserve_aspect=FALSE)) 


mat <- t(1L - 1L * (img[[1]][1,,] > 180))
mat_df <-data.frame(mat)

some_text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris eu vehicula sem, vitae vulputate dui. Pellentesque at imperdiet nulla. Donec bibendum ante lectus. Phasellus congue tellus nec velit eleifend eleifend. Fusce id eleifend ipsum. Nulla sed efficitur mi, at lobortis justo. Aliquam aliquam magna et felis condimentum scelerisque. Curabitur sapien risus, fringilla in scelerisque id, euismod quis ante. Proin id arcu eu nunc imperdiet sollicitudin. Pellentesque nec odio eget risus dapibus pretium. Vestibulum in ultricies elit.
Vivamus malesuada fermentum libero, id vehicula justo. Maecenas sit amet dui eget mauris tincidunt laoreet ac nec arcu. Integer nec rhoncus orci. Nam felis mauris, cursus id purus id, pretium facilisis lacus. Aliquam id consequat ex, vitae iaculis turpis. Maecenas eu lorem quis tortor imperdiet bibendum at ut tortor. Quisque iaculis aliquam mauris, quis sagittis libero pulvinar a. In vel magna non magna egestas blandit. Nulla ipsum tellus, pellentesque sed justo id, pellentesque fermentum libero. Nullam eu iaculis justo. In consectetur sagittis massa, eu aliquet sapien facilisis at. Nunc blandit tristique massa. Maecenas sed purus vel enim rutrum commodo.
Interdum et malesuada fames ac ante ipsum primis in faucibus. Quisque vitae mi eget nibh laoreet consectetur. Proin congue nibh ante, in sagittis arcu fringilla vitae. Vestibulum at sem sed ligula gravida consequat id vitae sapien. Suspendisse diam lacus, elementum id pretium euismod, consequat a ex. Nulla maximus neque purus, condimentum condimentum odio consequat sed. Cras congue sodales lorem, sed aliquam nisl mollis vel. Aliquam malesuada in massa non imperdiet. Pellentesque consequat nulla eu enim dictum sagittis. Quisque in vestibulum velit. Cras laoreet efficitur fringilla. Sed a elit tellus. Donec a ullamcorper est.
Curabitur tincidunt lacus sit amet congue ultrices. Praesent eget ante ligula. Nam faucibus congue massa, sit amet mattis erat porta nec. Cras lacinia tempus volutpat. Nam eget neque vel urna rhoncus sagittis eget ac dui. Aenean ultrices ut nibh vestibulum pulvinar. Suspendisse suscipit lorem ac blandit sollicitudin. Suspendisse et molestie nunc. Ut scelerisque arcu tellus, eu rutrum sapien suscipit et. Curabitur pellentesque rutrum ullamcorper. In ac magna ultricies, imperdiet elit sit amet, consequat dui. Mauris eleifend congue enim id convallis. Fusce egestas felis eros, eu finibus nisl accumsan bibendum. Ut odio tortor, posuere ut varius nec, congue ut metus. Integer molestie dolor et dui blandit, a rutrum justo dignissim.
Sed interdum urna sit amet sem venenatis cursus eget sed erat. Ut mattis sollicitudin magna a ornare. Nunc sodales ornare mattis. Phasellus ultricies, ante id dictum fermentum, purus turpis fermentum nunc, sit amet feugiat mauris magna in dolor. Nam dapibus ex nunc, eu auctor dui bibendum at. Nunc sed venenatis dolor. Suspendisse dignissim semper purus, vitae commodo metus interdum in. Suspendisse ullamcorper porta pellentesque."



dff <- data.frame(x = NULL, y = NULL)
for (i in 1:nrow(mat_df)) {
  for (j in 1:ncol(mat_df)){
    if (mat_df[i,j] == 1){
      d <- data.frame(x=i, y=j)
      dff <<- rbind(dff, d)
    }
  }
}

ggplot2::ggplot(dff)

