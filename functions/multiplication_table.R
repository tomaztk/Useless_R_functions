# multiplication table 10x10

library(tidyverse)

multiplication_table <- tibble(
  x = 1:10,
  y = 1:10
) %>% 
  complete(x, y) %>% 
  mutate(product = x * y)

ggplot(multiplication_table, aes(x, y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = product), color = "white") +
  scale_y_continuous(trans = "reverse") +
  coord_fixed() +
  theme_bw() + theme_void()
#  theme_void()
