# Collatz Conjecture


library(ggplot2)


collatz_sequence <- function(n) {
  if (n <= 0) stop("Input must be a positive integer.")
  sequence <- n
  while (n != 1) {
    n <- ifelse(n %% 2 == 0, n / 2, 3 * n + 1)
    sequence <- c(sequence, n)
  }
  return(sequence)
}

# Collatz Explorer 
collatz_explorer <- function(start, output_file = NULL) {
  collatz_seq <- collatz_sequence(start)
  print(collatz_seq)
  data <- data.frame(
    step = seq_along(collatz_seq),
    value = collatz_seq
  )
  print(head(data, 100))
  p <- ggplot(data, aes(x = step, y = value)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    scale_y_log10() +  # log
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    ) +
    labs(
      title = "Collatz Explorer",
      subtitle = paste("Starting number:", start),
      x = "Step",
      y = "Value (Log Scale)"
    )
  
  
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 8, height = 6)
    message("Plot saved to ", output_file)
  } else {
    print(p)
  }
}

# Example usage
collatz_explorer(604050036)
