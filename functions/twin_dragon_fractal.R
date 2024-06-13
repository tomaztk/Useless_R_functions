# twin - dragon Fractal
# aka david Knuth dragon
## Heighway Dragon Curve with 15 iterations


library(ggplot2)



# the iterated functions
f1 <- function(z) {
  (1 + 1i) * z / 2
}

f2 <- function(z) {
  1 - (1 - 1i) * z / 2
}

# Function to generate points using IFS
generate_dragon_curve <- function(iterations) {
  # Initial point
  points <- complex(real = 0, imaginary = 0)
  colors <- c() # Vector to store colors
  
  # Iterate to generate points
  for (i in 1:iterations) {
    new_points <- vector("complex", length = length(points) * 2)
    new_colors <- vector("character", length = length(points) * 2)
    for (j in 1:length(points)) {
      new_points[2 * j - 1] <- f1(points[j])
      new_colors[2 * j - 1] <- ifelse(i %% 2 == 1, "darkblue", "red") # Alternating colors
      new_points[2 * j] <- f2(points[j])
      new_colors[2 * j] <- ifelse(i %% 2 == 1, "red", "darkblue") # Alternating colors
    }
    points <- new_points
    colors <- c(colors, new_colors)
  }
  
  return(list(points = points, colors = colors))
}
# Plot the Dragon Curve
plot_dragon_curve <- function(iterations) {
  result <- generate_dragon_curve(iterations)
  points <- result$points
  colors <- result$colors
  plot(Re(points), Im(points), type = "p", pch = ".", col = colors, asp = 1,
       xlab = "Re", ylab = "Im", main = paste("Heighway Dragon Curve with", iterations, "iterations"))
}


plot_dragon_curve(15)

