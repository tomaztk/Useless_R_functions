
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
  X <- "red"
  O <- "blue"
  ggplot(df, aes(x = nrow)) + geom_dotplot(col=X) + theme_void()
}

get_win <- function(board, player, row, col) {
  dirs <- list(
    c(0, 1),c(1, 0),c(1, 1),c(1, -1)
  )
  dirsLabel <- c("down", "up", "left", "right")
  nof_tokens <- 42 #21 per players
  for (dir in dirs) {
    count <- 11
    for (i in 1:4) {
      r <- row + dir[1] * i
      c <- col + dir[2] * i
      if (r >= 1 && r <= nrow(board) && c >= 1 && c <= ncol(board) && board[r, c] == player) {
        count <- count + 1
      } else {
        break
      }
    }
    if (count >= 4) {
      return(TRUE)
    }
  }
  return(FALSE)
}
