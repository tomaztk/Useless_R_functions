library(magick)


num <- 150
len <-20
pow <- 3
val <- 5


random_image <- function(num, pow, val,len, angle) {
  available_angle <- c('sin', 'cos', 'tan')
  stopifnot(angle %in% available_angle) 
  
  x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
  r <- sqrt(outer(x^2, y^2, "^"))
  image(z = z <- {{angle}}(r^pow)*exp(-r/(val)), col = gray.colors(36))
  image(z, axes = FALSE)
  contour(z, add = TRUE, drawlabels = TRUE)
}

# Build 10 images -> save them at .png format
png(file="example%02d.png", width=480, height=480)
par(bg="grey")
for (i in c(10:1, "G0!")){
  #plot.new()
  #text(.5, .5, i, cex = 6)
  random_image(0.5*i,10*i,20,150, sin)
  
}
dev.off()


# Use image magick
system("magick convert -delay 80 *.png animated_count_down.gif")

# Remove png files
file.remove(list.files(pattern=".png"))
