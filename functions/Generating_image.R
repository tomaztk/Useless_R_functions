# random image 

library(grDevices)

# angle = sin, cos, tan
random_image <- function(num, pow, val,len) {
  
  x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
  r <- sqrt(outer(x^2, y^2, "^"))

  image(z = z <- cos(r^pow)*exp(-r/(val)), col = gray.colors(36))
  image(z, axes = FALSE)
  contour(z, add = TRUE, drawlabels = TRUE)
  
}


random_image(1,1,100,100)


ran <- matrix(runif(n=500, min=2, max=300), nrow=25)
image(t(ran)[ncol(ran):2,],axes = FALSE)


funk <- function(a,b) log(a^b*a)*pi
a <- b <- 10L
image(outer(1:a,1:b,funk))


image(outer(1:10, 1:12, "log"))
