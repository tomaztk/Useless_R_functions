num <-200 
len <- 20

x <-  seq((-num)*pi, (num)*pi, length.out = len)
y <- seq((-num)*pi, num*pi, length.out = len)
r <- sqrt(outer(x^2, y^2, "^"))
image(z = z <- sin(r^0.3)*exp(-r/(10)), col = gray.colors(36))


num <- 150
len <-20
pow <- 3
val <- 5
angle <- 'sin'

stopifnot(angle %in% available_angle) {
  x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
  r <- sqrt(outer(x^2, y^2, "^"))
  image(z = z <- {{angle}}(r^pow)*exp(-r/(val)), col = gray.colors(36))
  image(z, axes = FALSE)
  contour(z, add = TRUE, drawlabels = TRUE)
}


random_image <- function(num, pow, val,len, angle) {
  available_angle <- c('sin', 'cos', 'tan')
  stopifnot(angle %in% available_angle) 
    
    x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
    r <- sqrt(outer(x^2, y^2, "^"))
    image(z = z <- {{angle}(r^pow)*exp(-r/(val)), col = gray.colors(36))
    image(z, axes = FALSE)
    contour(z, add = TRUE, drawlabels = TRUE)
}

# runfun
random_image(0.5,0.1,20,150, cos)
random_image(0.5,10,20,150, sin)





# aa <- runif(10,10,kingdom)


random_image <- function(num, pow, val,len, angle='sin') {
  available_angle <- c('sin', 'cos', 'tan')
  stopifnot(angle %in% available_angle) 
  
  x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
  r <- sqrt(outer(x^2, y^2, "^"))
  image(z = z <- {{angle}}(r^pow)*exp(-r/(val)), col = gray.colors(36))
  image(z, axes = FALSE)
  contour(z, add = TRUE, drawlabels = TRUE)
  
}

random_image(10,2,2,1)


stopifnot(angle %in% available_angle) {
  x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
  r <- sqrt(outer(x^2, y^2, "^"))
  image(z = z <- {{angle}}(r^pow)*exp(-r/(val)), col = gray.colors(36))
  image(z, axes = FALSE)
  contour(z, add = TRUE, drawlabels = TRUE)
}



