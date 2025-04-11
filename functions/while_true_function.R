getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")


##########################################
# 
# Home made while_true function
# that is loop
#
# Series:
# Little Useless-useful R functions #67
# Created: April 11, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


while_true <- function(seed = NULL, verbose = TRUE, plot = TRUE, max_iter = 1000) {
  if (!is.null(seed)) set.seed(seed)
  
  x <- runif(1, 1, 100)
  path <- numeric()
  i <- 0
  
  is_magical <- function(x) {
    s <- format(round(x, 6), scientific = FALSE)
    gsub("\\.", "", s) |> strsplit("") |> unlist() -> chars
    return(length(unique(chars)) < 4 || paste0(rev(chars), collapse = "") == paste0(chars, collapse = ""))
  }
  
  if (verbose) cat("🌌 Entering the loop of chaos...\n")
  
  while (TRUE) {
    i <- i + 1
    if (i > max_iter) {
      cat("😵 Escaped due to boredom. No enlightenment found.\n")
      break
    }
    
    path <- c(path, x)
    
    if (verbose) cat(sprintf("Iteration %03d: x = %.5f\n", i, x))
    
    if (is_magical(x)) {
      cat("✨ A beautiful number has emerged. We are free.\n")
      break
    }
    
    # Absurd evolution
    x <- abs(sin(x) * log(x + 1) + sqrt(x) * runif(1, -0.5, 0.5))
  }
  
  if (plot) {
    library(ggplot2)
    df <- data.frame(iter = seq_along(path), value = path)
    ggplot(df, aes(iter, value)) +
      geom_line(color = "skyblue", linewidth = 1.2) +
      geom_point(color = "steelblue", size = 1.5) +
      theme_minimal() +
      labs(
        title = "🌀 while_true(): The Number's Journey to Enlightenment",
        x = "Iteration",
        y = "Value of x"
      )
  }
  
  invisible(path)
}


# function usage
while_true(seed = 42)



#adding animations
# install.packages(c("ggplot2", "gganimate", "transformr"))


library(ggplot2)
library(gganimate)

while_true_animated <- function(seed = NULL, verbose = TRUE, max_iter = 100) {
  if (!is.null(seed)) set.seed(seed)
  
  x <- runif(1, 1, 100)
  path <- data.frame(
    iter = 1,
    x = x,
    sin_x = sin(x),
    log_x = log(x),
    magical = FALSE
  )
  i <- 1
  
  is_magical <- function(x) {
    s <- format(round(x, 6), scientific = FALSE)
    chars <- unlist(strsplit(gsub("\\.", "", s), ""))
    return(length(unique(chars)) < 4 || paste0(rev(chars), collapse = "") == paste0(chars, collapse = ""))
  }
  
  if (verbose) cat("🌌 Starting animated journey...\n")
  
  while (TRUE) {
    i <- i + 1
    if (i > max_iter) {
      if (verbose) cat("😵 Exiting after max iterations. No enlightenment found.\n")
      break
    }
    
    x <- abs(sin(x) * log(x + 1) + sqrt(x) * runif(1, -0.5, 0.5))
    
    magical <- is_magical(x)
    path <- rbind(path, data.frame(
      iter = i,
      x = x,
      sin_x = sin(x),
      log_x = log(x),
      magical = magical
    ))
    
    if (magical) {
      if (verbose) cat("✨ Magic number achieved. Ending journey at iteration", i, "\n")
      break
    }
  }
  

  p <- ggplot(path, aes(x = x, y = sin_x)) +
    geom_path(aes(group = 1), color = "grey70", linewidth = 0.8, alpha = 0.6) +
    geom_point(aes(size = log_x, color = magical), alpha = 0.9) +
    scale_color_manual(values = c("steelblue", "gold")) +
    scale_size_continuous(range = c(1, 6)) +
    labs(
      title = "🌀 while_true_animated(): Enlightenment Path",
      subtitle = "Iteration: {frame}",
      x = "x",
      y = "sin(x)",
      color = "Magical?",
      size = "log(x)"
    ) +
    theme_minimal(base_size = 14)
  
  anim <- p + transition_reveal(iter) +
    ease_aes("cubic-in-out")
  
  animate(anim, nframes = nrow(path) * 5, fps = 10, width = 700, height = 500)
}


while_true_animated(seed = 42)
