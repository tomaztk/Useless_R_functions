library(ggplot2)
library(dplyr)
library(plotly)       # For interactive 3D plots
library(gganimate)    # For animations
library(transformr)
library(gifski)

# Function to generate the Collatz sequence
collatz_sequence <- function(n) {
  if (n <= 0) stop("Input must be a positive integer.")
  sequence <- n
  while (n != 1) {
    n <- ifelse(n %% 2 == 0, n / 2, 3 * n + 1)
    sequence <- c(sequence, n)
  }
  return(sequence)
}

collatz_sequence(240)

# Function to visualize the Collatz sequence in 2D
collatz_explorer <- function(start, output_file = NULL) {
  collatz_seq <- collatz_sequence(start)
  data <- data.frame(
    step = seq_along(collatz_seq),
    value = collatz_seq
  )
  
  p <- ggplot(data, aes(x = step, y = value)) +
    geom_line(color = "blue", size = 1) +
    geom_point(aes(color = value), size = 3) +
    scale_color_gradient(low = "red", high = "green") +
    theme_minimal() +
    labs(
      title = paste("Collatz Explorer - Starting Number:", start),
      subtitle = "A chaotic yet strangely ordered path to 1",
      x = "Step",
      y = "Value"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 8, height = 6)
    message("Plot saved to ", output_file)
  } else {
    print(p)
  }
}


# Function for a chaotic 3D plot
collatz_3D <- function(start_values = c(7, 15, 27, 97, 123)) {
  all_data <- data.frame()
  
  for (start in start_values) {
    seq_data <- collatz_sequence(start)
    df <- data.frame(
      step = seq_along(seq_data),
      value = seq_data,
      start_value = start
    )
    all_data <- rbind(all_data, df)
  }
  
  fig <- plot_ly(all_data, x = ~step, y = ~value, z = ~start_value,
                 type = 'scatter3d', mode = 'lines+markers',
                 marker = list(size = 3, color = ~start_value, colorscale = 'Viridis'),
                 line = list(width = 2)) %>%
    layout(title = "Chaotic Collatz 3D Plot",
           scene = list(
             xaxis = list(title = "Step"),
             yaxis = list(title = "Value"),
             zaxis = list(title = "Start Value")
           ))
  
  fig
}

# Function to create an animated visualization
collatz_animation <- function(start, output_file = "collatz_animation.gif") {
  collatz_seq <- collatz_sequence(start)
  data <- data.frame(
    step = seq_along(collatz_seq),
    value = collatz_seq
  )
  
  # Create animation with proper grouping
  p <- ggplot(data, aes(x = step, y = value, group = 1 ))  
    geom_line(color = "blue", size = 1) +
    geom_point(aes(color = value), size = 4) +
    scale_color_gradient(low = "red", high = "green") +
    theme_minimal() +
    transition_reveal(step)   
  
  # Save animation
  anim_save(output_file, animation = p, duration = 5, fps = 10, width = 8, height = 6, renderer = gifski_renderer())
  message("Animation saved as ", output_file)
}

collatz_animation(start=30)



# Example Usage:
collatz_explorer(27)        # 2D visualization
collatz_explorer(200)        # 2D visualization
collatz_explorer(1200)        # 2D visualization
collatz_3D(c(700, 1500, 2700))    # 3D chaotic visualization
collatz_animation(127)       # Animated version

#getwd()
#setwd("/Users/tomazkastrun/Documents")


collatz_explorer(240671)
collatz_explorer(2000000)
