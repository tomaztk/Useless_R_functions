
### Fractals


library(ggplot2)
library(dplyr)

applysapply_fractal_tree <- function(depth = 5, angle = 20, length = 1) {

  
  grow_branch <- function(x, y, angle, length, depth) {
    if (depth == 0) return(data.frame())
    
    # Compute end coordinates of the branch
    xend <- x + length * cospi(angle / 180)
    yend <- y + length * sinpi(angle / 180)
    
    branch <- data.frame(
      x = x, y = y,
      xend = xend, yend = yend,
      level = depth
    )
    
    # left /  right :)
    directions <- c(-1, 1)
    children <- apply(matrix(directions), 1, function(d) {
      sapply(1, function(dummy) {
        grow_branch(xend, yend, angle + d * 30, length * 0.7, depth - 1)
      }, simplify = FALSE)
    })
    
    all_branches <- do.call(rbind, c(list(branch), unlist(children, recursive = FALSE)))
    return(all_branches)
  }
  

  tree_data <- grow_branch(0, 0, 90, length, depth)
  

  ggplot(tree_data) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = as.factor(level)),
                 lineend = "round", linewidth = 0.8) +
    scale_color_viridis_d(option = "D") +
    theme_void() +
    coord_fixed() +
    labs(
      title = "applysapply_fractal_tree()",
      subtitle = "Recursively grown tree using apply() and sapply()",
      color = "Depth Level"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

#test
applysapply_fractal_tree(depth = 13, angle = 54, length = 1)

#applysapply_fractal_tree()
