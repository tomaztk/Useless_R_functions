
iris <- iris
#iris <- iris$Species

lapply(iris, view)

apply(lapply(iris, view), 2,sum)


apply(iris[1:4], 2,sum)

apply(iris[1:4], 1,sum)


# group by apply
library(dplyr)
iris %>%
  group_by(Species) %>%
  group_map(~ head(.x, 3L))
