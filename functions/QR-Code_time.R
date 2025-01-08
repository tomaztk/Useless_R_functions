##########################################
# 
# QR-Code Clock
#
# Series:
# Little Useless-useful R functions #67
# Created: January 08, 2035
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################



library(qrcode)


while (Sys.time()+1 > Sys.time()){
  n_o_w <- paste0("Current time is ",as.character(format(Sys.time(), "%X")))
  print(n_o_w)
  qr_code(n_o_w, ecl = "M" ) |>
    plot()
  Sys.sleep(10)
}



# creating animation

library(gganimate)

getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/figures")

for (i in 1:10){
   n_o_w <- paste0("Current time is ",as.character(format(Sys.time(), "%X")))
   print(n_o_w)
   qr <- qr_code(n_o_w, ecl = "M" )
   filename <- paste0("QRCode", i, ".png")
   png(filename)
   plot(qr)
   dev.off()
  Sys.sleep(1)
}

library(magick)

files <- c("QRCode1.png", "QRCode2.png", "QRCode3.png", "QRCode4.png", "QRCode5.png", "QRCode6.png", "QRCode7.png", "QRCode8.png", "QRCode9.png", "QRCode10.png")
images <- image_read(files)
animation <- image_animate(images, fps = 1)  
image_write(animation, "QRCode_animation.gif")
