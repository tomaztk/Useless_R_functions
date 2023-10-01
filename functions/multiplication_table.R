################
# multiplication 
# table 10x10
################

library(tidyverse)

tibble(x=1:10, y = 1:10) %>%
  complete(x,y) %>%
  mutate(prod = x*y) %>%
  ggplot( aes(x, y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = prod), color = "white") +
  scale_y_continuous(trans = "reverse") +
  coord_fixed() +
  theme_bw() + theme_void()




