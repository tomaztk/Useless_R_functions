### not yet snow

# ToDo: create animation with alternating / jitter flakes

set.seed(2908)
n <- 1000 

aa <- data.frame(x = runif(n),  
       y = runif(n),  
       size = runif(n, min = 4, max = 20),
       run = sample.int(10, 10)) 


ggplot(aa, aes(x, y, size = size)) +
      geom_point(color = "white", pch = 58, alpha=3/5) + # pch = 8 
      scale_size_identity() +
      theme_void() +
      theme(panel.background = element_rect("black"))

#animate -> lame :S
library(gganimate)

snow <- ggplot(aa, aes(x, y, size = size)) +
  geom_point(color = "white", pch = 8, alpha=3/5) + # pch = 8
  scale_size_identity() +
  theme_void() +
  theme(panel.background = element_rect("black"))


# Running animation
library(gganimate)
snow +
  transition_time(run)

