
# load ggplot2
##########################################
# 
# Making game 4-in-a-row
#
# Series:
# Little Useless-useful R functions #55
# Created: October 07, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

library(ggplot2)

init_board <- function(r,c) {
  matrix(" ", nrow = r, ncol = c)
}

print_board <- function(board) {
  df <- data.frame(matrix)
  ggplot(df, aes(x = nrow)) + geom_dotplot(col="red") + theme_void()
}




