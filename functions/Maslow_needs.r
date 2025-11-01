##########################################
# 
# Useless Pyramid of R needs
# Adopted by Maslow's hierarchy of needs
#
# Series:
# Little Useless-useful R functions #81
# Created: November  1, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

###########################################

library(ggplot2)
library(grid)

useeless_needs_pyramid <- function(
    levels = c("L1","L2", "L3", "L4", "L5"),
    palette = c("#8e24aa","#3f51b5","#2196f3","#4caf50","#fbc02d")
) {

  h <- 1
  widths <- seq(1, 0.4, length.out = 5)
  df <- data.frame(
    tier = 5:1,
    label = rev(levels),
    ymin = (0:4)*h,
    ymax = (1:5)*h,
    width = rev(widths),
    fill = palette
  )

  
  df$xmin <- -df$width/2
  df$xmax <-  df$width/2
  df$y    <- (df$ymin + df$ymax)/2
  df$ymin <- 5 - df$ymax
  df$ymax <- df$ymin + h
  df$y    <- (df$ymin + df$ymax)/2
  
  
  # wrap long captions
  wrap_width = 32
  wrap_fun <- function(s, w) paste(strwrap(s, width = w), collapse = "\n")
  df$label_wrapped <- vapply(df$label, wrap_fun, character(1), w = wrap_width)
  df$label_x <- df$xmin + 0.02
  

  
  # helper for RGBA color
  add_alpha <- function(col, alpha) {
    rgb <- grDevices::col2rgb(col, TRUE)/255
    grDevices::rgb(rgb[1], rgb[2], rgb[3], alpha = alpha)
  }
  
  p <- ggplot(df) +
    geom_rect(
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=I(fill)),
      color = "white", linewidth = 1
    )  + geom_label(
      aes(x = label_x, y = y, label = label_wrapped),
      hjust = 0, vjust = 0.5,
      label.size = 0,
      fill = add_alpha("white", 0.2),
      label.padding = unit(6, "pt"),
      size = 5,
      lineheight = 1.05,
      color = "black"
    )

  
  p <- p +
    coord_cartesian(xlim = c(-0.8, 0.6), ylim = c(0,5), expand = FALSE) +
    labs(title   = "Useless Pyramid of R needs",
         subtitle= "From useless fo useful R functions",
         x = NULL, y = NULL) +
    theme_void(base_size = 13) +
    theme(
      plot.background = element_rect(colour = "gray"), 
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(margin = margin(t = 4,b = 10), hjust = 0.5),
      panel.background = element_rect(fill = "#0f0f12", color = NA),
      plot.margin = margin(20,20,20,20)
    )
  print(p)

}
 

# Run fctnioin
useeless_needs_pyramid(
  levels = c(
    "Gathering ideas for writing useless-useful R functions",
    "Applying interesting math problems",
    "Writing quarky and niffty R code",
    "Getting new knowledge",
    "Engaging community"
  ),
  palette = c("mediumpurple","yellowgreen","yellow","gold","red")
)


