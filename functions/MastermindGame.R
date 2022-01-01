
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
  
  # adding boarders 
  par(new = TRUE)
  plot(c(100,500), c(100,500), xlab = "", ylab = "",  xaxt = 'n', yaxt = 'n',main = "Mastermind board game")
  
  nof_tries <- 10
  
  for (i in 1:nof_tries) {  #rows
    #i <- 30*(0:nof_tries)
    #print(i)
    for (j in 1:nof_col) {  #columns
      col <- 50*(1:nof_col-1)
      # 6 per row | every second row empyt | start top - down
      rect(100+col[j], 500-(i*30), 150+col[j], 475-(i*30), col = 'white')
    }
    
    #key pegs
    h <- 3
    for (h in 0:3) {
      if (h == 0) {
        par(new = TRUE)
        plot(450,500-(i*30), col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
      }
      if (h == 1) {
        par(new = TRUE)
        plot(450,490-(i*30), col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
      }
      if (h == 2) {
        par(new = TRUE)
        plot(470,500-(i*30), col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
      }
      if (h == 3) {
        par(new = TRUE)
        plot(470,490-(i*30), col = "black", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
      }
    }
  }
}


plot.new()
get_board(nof_col=6,nof_pegs=4)

game <- function(){
  x11()
  plot.new()
  get_board(nof_col=6,nof_pegs=4)
  #colours selection
  colours = c('Red','Green','Blue','Yellow','Brown','Orange','Magenta')
  for (z in 1:6) {
    rect(100+z*50, 100, 150+z*50, 175, col = colours[z])
  }
  mouse.at <- locator(n = 1, type = "o") 
  x.at <<- mouse.at$x
  y.at <<- mouse.at$y
  graphics.off()
}

game()



