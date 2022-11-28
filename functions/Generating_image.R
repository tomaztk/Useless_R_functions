##########################################
#
# Random image 
#
#
# Series:
# Little Useless-useful R functions #44
# Created: November 25, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################


### Function 1 - using trigon. angle func.

# angle = sin, cos, tan
random_image <- function(num, pow, val,len, angle) {
  available_angle <- c('sin', 'cos', 'tan')
  stopifnot(angle %in% available_angle) {
    x <- y <- seq((-num)*pi, (num)*pi, length.out = len)
    r <- sqrt(outer(x^2, y^2, "^"))
    image(z = z <- {{angle}}(r^pow)*exp(-r/(val)), col = gray.colors(36))
    image(z, axes = FALSE)
    contour(z, add = TRUE, drawlabels = TRUE)
  }
}

# runfun
random_image(0.5,0.1,20,150, cos)
random_image(0.5,10,20,150, sin)


### 2. Simplified versions

image(matrix(runif(50*50), ncol=50))
image(outer(1:10, 1:12, "log"))
image(outer(1:10, 1:12, "*"), useRaster = TRUE)


### 3. Plotting useless statistics

iris <- iris
image(outer(iris$Sepal.Length, iris$Petal.Length))
image(outer(iris$Sepal.Length, iris$Sepal.Width))

