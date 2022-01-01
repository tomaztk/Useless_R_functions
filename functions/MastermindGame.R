
##########################################
# 
# Mastermind game
#
# Mastermind or Master Mind is a code-breaking game for two players.  
# The game is played using:
# - a decoding board, with a shield at one end covering a row of four large holes, and twelve (or ten, or eight, or six) additional 
#   rows containing four large holes next to a set of four small holes;
# - code pegs of six different colors (or more; see Variations below), with round heads, which will be
#   placed in the large holes on the board; and
# - key pegs, some colored black, some white, which are flat-headed and smaller than the code pegs; 
#   they will be placed in the small holes on the board.
#   URL: https://en.wikipedia.org/wiki/Mastermind_(board_game)

# Series:
# Little Useless-useful R functions #31
# Created: December 31, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

numberOfPegs <- 4
numberOfColors <- 6


get_secret <- function(nof_col, nof_pegs, colours_repeat=TRUE) {
  if( is.null(colours) ) {
    colours = c('Red','Green','Blue','Yellow','Brown','Orange','Magenta')
  }
  col = sample(colours, nof_col) 
  sample(col,nof_pegs, replace=colours_repeat)
}

# store secret
s <- get_secret(nof_col=3, nof_pegs=5)


get_board <- function(nof_col, nof_pegs){
  
  # Plot empty board for mastermind
  plot.new()
  op <- par(bg = "white")
  grid(nx = 6, ny = 12, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  
  # adding layers of 
  par(new = TRUE)
  #plot(c(100,300), c(200,500), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', main = "Mastermind board game") #, ann = FALSE) #, frame.plot=FALSE)
  plot(c(100,500), c(100,500), xlab = "", ylab = "",  xaxt = 'n', yaxt = 'n',main = "Mastermind board game")
  
  # code Pegs
  i <- 6*(0:10)
  # 6 per row | every second row empyt | start top - down
  rect(100, 500, 150, 475, col = 'Red')
  rect(150, 500, 200, 475, col = 'Blue')
  rect(200, 500, 250, 475, col = 'Green')
  rect(250, 500, 300, 475, col = 'Yellow')
  rect(300, 500, 350, 475, col = 'Yellow')
  rect(350, 500, 400, 475, col = 'Magenta')
  
  # 6 per row | every second row empyt | start top - down
  rect(100, 470, 150, 445, col = 'blue')
  rect(150, 470, 200, 445, col = 'Green')
  rect(200, 470, 250, 445, col = 'Red')
  rect(250, 470, 300, 445, col = 'Red')
  rect(300, 470, 350, 445, col = 'Magenta')
  rect(350, 470, 400, 445, col = 'Magenta')
  
  
  # Key Pegs
  par(new = TRUE)
  plot(450,500, col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(450,490, col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(470,490, col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(470,500, col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  
  par(new = TRUE)
  plot(450,460, col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(450,450, col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(470,450, col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  par(new = TRUE)
  plot(470,460, col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  
}




