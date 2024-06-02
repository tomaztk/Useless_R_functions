library(ggplot2)

my_palette <- c("red", "limegreen", "#3357FF", "goldenrod1", "#33FFFF", "brown")

set.seed(2908)

# make sample data
data <- data.frame(
  x = 1:25,
  y = rnorm(25),
  group = rep(c("A", "B", "C", "D", "E"), each = 5)
)

# scatter
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point(size = 3) +
  scale_color_manual(values = my_palette, na.value = "grey45") +
  theme_minimal()

# barchart
ggplot(data, aes(x = x, y = y, color = group, fill=group)) +
  geom_bar(stat = "identity") +
  scale_color_manual(values = my_palette, na.value = "grey45") +
  theme_minimal()


# boxplot
ggplot(data, aes(x = x, y = y,  fill=group)) +
  geom_boxplot() +
  scale_color_manual(values = my_palette, na.value = "grey45") +
  theme_minimal()


data2 <- data.frame(
  y = c(2,15,24,9,17,2),
  group = LETTERS[1:6])
)

# 3.14chart
ggplot(data2, aes(x='', y=y, fill=group)) +
  geom_bar(stat="identity", width=1, colour="white") +
  scale_color_manual(values = my_palette, na.value = "grey45") +
  coord_polar("y", start=0) + 
  theme_void()
