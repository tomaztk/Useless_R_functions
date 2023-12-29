library(ggplot2)

# Useless function of a house
draw_2d_house <- function() {
  ggplot() +
    geom_rect(aes(xmin = 1, xmax = 4, ymin = 1, ymax = 4, fill = "lightblue"), color = "black") +
    geom_polygon(aes(x = c(0.5, 2.5, 4.5), y = c(4, 6, 4)), fill = "red", color = "black") +
    geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 1, ymax = 2, fill = "brown"), color = "black") +
    
    # Draw the window
    geom_rect(aes(xmin = 1.5, xmax = 2, ymin = 3, ymax = 3.5, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 3, xmax = 3.5, ymin = 3, ymax = 3.5, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 1.5, xmax = 2, ymin = 3.5, ymax = 4, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 3, xmax = 3.5, ymin = 3.5, ymax = 4, fill = "yellow"), color = "black") +
    
    coord_fixed(ratio = 1, xlim = c(0, 5), ylim = c(0, 7)) +
    theme_void() + theme(legend.position = "none") +     ggtitle("This is my house")
}

# run function
draw_2d_house()

# Useless function of a house with a tree :)

draw_2d_house_and_tree <- function() {
  ggplot() +
    geom_rect(aes(xmin = 1, xmax = 4, ymin = 1, ymax = 4, fill = "lightblue"), color = "black") +
    geom_polygon(aes(x = c(0.5, 2.5, 4.5), y = c(4, 6, 4)), fill = "red", color = "black") +
    geom_rect(aes(xmin = 2.5, xmax = 3.5, ymin = 1, ymax = 2, fill = "brown"), color = "black") +
    geom_rect(aes(xmin = 1.5, xmax = 2, ymin = 3, ymax = 3.5, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 3, xmax = 3.5, ymin = 3, ymax = 3.5, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 1.5, xmax = 2, ymin = 3.5, ymax = 4, fill = "yellow"), color = "black") +
    geom_rect(aes(xmin = 3, xmax = 3.5, ymin = 3.5, ymax = 4, fill = "yellow"), color = "black") +
    
    # Treeee
    geom_rect(aes(xmin = 5, xmax = 5.5, ymin = 1, ymax = 3, fill = "brown"), color = "black") +
    geom_polygon(aes(x = c(4.2, 5.75, 6.75, 3.25), y = c(3, 3.5, 2.5, 2)), fill = "green", color = "black") +
    geom_polygon(aes(x = c(4, 4.5, 5.5, 6, 5), y = c(3.5, 4.5, 4.5, 3.5, 3)), fill = "green", color = "green") +
    geom_polygon(aes(x = c(4, 4.5, 5.5, 6, 5), y = c(2.5, 3.5, 3.5, 2.5, 2.5)), fill = "green", color = "green") +
    

    coord_fixed(ratio = 1, xlim = c(0, 7), ylim = c(0, 7)) +
    theme_void() + theme(legend.position = "none") + ggtitle("This is my house and Tree")
}

draw_2d_house_and_tree()
