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

install.packages(c("ggplot2", "gganimate", "transformr"))
