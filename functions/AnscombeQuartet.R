
# Anscombe's Quartet

library(ggplot2)
library(grid)
library(gridExtra)
library(datasets)


anscombe <- datasets::anscombe


p1 <- ggplot(anscombe) +
  geom_point(aes(x1, y1), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x1", y = "y1",
       title = "Dataset 1" ) +
  theme_bw()
p1


p2 <- ggplot(anscombe) +
  geom_point(aes(x2, y2), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x2", y = "y2",
       title = "Dataset 2" ) +
  theme_bw()
p2


p3 <- ggplot(anscombe) +
  geom_point(aes(x3, y3), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x3", y = "y3",
       title = "Dataset 3" ) +
  theme_bw()
p3

p4 <- ggplot(anscombe) +
  geom_point(aes(x4, y4), color = "darkorange", size = 1.5) +
  scale_x_continuous(breaks = seq(0,20,2)) +
  scale_y_continuous(breaks = seq(0,12,2)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "x4", y = "y4",
       title = "Dataset 4" ) +
  theme_bw()
p4


grid.arrange(grobs = list(p1, p2, p3, p4), 
             ncol = 2, 
             top = "Anscombe's Quartet")



p1_fitted <- p1 + geom_abline(intercept = 3.0001, slope = 0.5001, color = "blue")
p2_fitted <- p2 + geom_abline(intercept = 3.001, slope = 0.500, color = "blue")
p3_fitted <- p3 + geom_abline(intercept = 3.0025, slope = 0.4997, color = "blue")
p4_fitted <- p4 + geom_abline(intercept = 3.0017, slope = 0.499, color = "blue")

grid.arrange(grobs = list(p1_fitted, p2_fitted,
                          p3_fitted, p4_fitted), 
             ncol = 2, 
             top = "Anscombe's Quartet")