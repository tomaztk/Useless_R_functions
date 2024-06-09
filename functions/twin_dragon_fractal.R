# twin - dragon Fractal
# aka david Knuth dragon


library(ggplot2)

# Define the function to generate Twin Dragon fractal
generate_twin_dragon <- function(iterations) {
  # Initialize points
  x <- c(0, 1)
  y <- c(0, 0)
  colors <- c(1, 2)  
  
  for (i in 1:iterations) {
    # Calculate new points
    new_x <- c()
    new_y <- c()
    new_colors <- c()
    for (j in 1:(length(x) - 1)) {
      x1 <- x[j]
      y1 <- y[j]
      x2 <- x[j + 1]
      y2 <- y[j + 1]
      # Add sin function
      new_x <- c(new_x, x1, (x1 + x2 - y1 + y2) / 2)
      new_y <- c(new_y, y1, (y1 + y2 + x1 - x2) / 2)
      new_colors <- c(new_colors, colors[j], ifelse(colors[j] == 1, 2, 1))
    }
    new_x <- c(new_x, tail(x, n = 1))
    new_y <- c(new_y, tail(y, n = 1))
    new_colors <- c(new_colors, tail(colors, n = 1))
    x <- new_x
    y <- new_y
    colors <- new_colors
  }
  
  return(data.frame(x, y, color = as.factor(colors)))
}

# Define the function to plot Twin Dragon fractal
plot_twin_dragon <- function(data) {
  ggplot(data, aes(x = x, y = y)) +
    geom_path() +
    coord_equal() +
    theme_void() +
    ggtitle("Twin Dragon Fractal")
}

# Generate the Twin Dragon fractal data
iterations <- 15
twin_dragon_data <- generate_twin_dragon(iterations)



# function
plot_twin_dragon(twin_dragon_data)



ggplot(twin_dragon_data, aes(x = x, y = y)) +
  geom_path() +
  coord_equal() +
  theme_void() +
  ggtitle("Twin Dragon Fractal")
