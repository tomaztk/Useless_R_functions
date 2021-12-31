
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
#        - add clean rings?
###########################################


# posibble options (with allowing duplicate colours) 
# 6**4

numberOfPegs <- 4
numberOfColors <- 6



all <- combn(LETTERS[1:numberOfColors], numberOfPegs, simplify = TRUE)
all2 <- gtools::permutations(4, numberOfPegs, LETTERS[1:numberOfColors], repeats.allowed=TRUE)


# Plot empty board for mastermind
plot.new()
grid(nx = 6, ny = 12, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)

# adding layers of 
par(new = TRUE)
plot(14,24, pch = 19, col = 4)








