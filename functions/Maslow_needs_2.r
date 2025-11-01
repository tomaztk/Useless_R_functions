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

useless_needs_pyramid <- function(
    levels  = c("L1","L2","L3","L4","L5"),
    palette = c("#8e24aa","#3f51b5","#2196f3","#4caf50","#fbc02d"),
    title   = "Useless Pyramid of R Needs",
    subtitle= "From useless to useful R functions",
  
    label_size = 5,
    label_lineheight = 1.05,
    label_text_color = "black",
    box_fill = "white",
    box_alpha = 0.22,
    left_pad = 0.05
) {

  tiers <- 5
  H  <- tiers * 1.0   
  BW <- 3.4            

  w_at <- function(y) (BW/2) * (1 - y / H)
  y_breaks <- seq(0, H, length.out = tiers + 1)
  poly_list <- lapply(seq_len(tiers), function(i) {
    ymin <- y_breaks[i]
    ymax <- y_breaks[i + 1]
    wt   <- w_at(ymax)
    wb   <- w_at(ymin)
    data.frame(
      tier = i,
      x = c(-wt, wt, wb, -wb),
      y = c(ymax, ymax, ymin, ymin)
    )
  })
  poly_df <- do.call(rbind, poly_list)

  # ToDO: Add delimiters for text!
  lab_df <- data.frame(
    tier  = seq_len(tiers),
    ymin  = y_breaks[-(tiers + 1)],
    ymax  = y_breaks[-1]
  )
  
  lab_df$y_mid <- (lab_df$ymin + lab_df$ymax) / 2
  lab_df$w_mid <- w_at(lab_df$y_mid)
  lab_df$label <- rev(levels)                
  lab_df$fill  <- palette
  lab_df$label_x <- -lab_df$w_mid + left_pad
  

  # wrap long labels
  wrap_width = 25
  wrap_fun <- function(s, w) paste(strwrap(s, width = w), collapse = "\n")
  lab_df$label_wrapped <- vapply(lab_df$label, wrap_fun, character(1), w = wrap_width)
  

  add_alpha <- function(col, alpha) {
    rgb <- grDevices::col2rgb(col, TRUE)/255
    grDevices::rgb(rgb[1], rgb[2], rgb[3], alpha = alpha)
  }
  
  poly_df <- merge(poly_df, transform(lab_df, tier = tier, fill = fill), by = "tier", sort = FALSE)
  
  p <- ggplot() +
    geom_polygon(
      data = poly_df,
      aes(x = x, y = y, group = tier, fill = I(fill)),
      color = "white", linewidth = 1
    ) +
    geom_label(
      data = lab_df,
      aes(x = label_x, y = y_mid, label = label_wrapped),
      hjust = 0, vjust = 0.5,
      label.size = 0,
      fill = add_alpha(box_fill, box_alpha),
      label.padding = unit(6, "pt"),
      size = label_size,
      lineheight = label_lineheight,
      color = label_text_color
    ) +
    coord_equal(
      xlim = c(-BW/2 - 0.2, BW/2 + 0.5), ylim = c(0, H),
      expand = FALSE
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_void(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(margin = margin(t = 4, b = 10), hjust = 0.5),
      panel.background = element_rect(fill = "#0f0f12", color = NA),
      plot.background  = element_rect(colour = "gray", fill = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  print(p)
}



useless_needs_pyramid(
  levels = c(
    "Engage \ncommunity",
    "Getting new knowledge",
    "Writing quarky and niffty R code",
    "Applying interesting math problems",
    "Gathering ideas for writing useless-useful R functions"
  ),
  #palette = c("mediumpurple","yellowgreen","yellow","gold","red")
  palette = c("red","gold","yellow","yellowgreen","mediumpurple")
)
