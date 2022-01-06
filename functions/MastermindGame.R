
##########################################
# 
# Mastermind board game for R Language
# 
# Game for single-player R developers/Data scientists for
# killing time, playing game while waiting for the ML
# model to finish training or just to play.
#
# Series:
# L ittle Useless-useful R functions #31
#  Created: January 06, 2022
#  Author: Tomaz Kastrun
#  Blog: tomaztsql.wordpress.com
#  V.1.0
# 
# Changelog:
# 
###########################################

numberOfColors <- 4
numberOfTries <- 10

get_board <- function(nof_col, nof_try=10){
  plot.new()
  op <- par(bg = "white")
  grid(nx = 6, ny = 12, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  
  # adding boarders 
  par(new = TRUE)
  plot(c(100,500), c(100,500), xlab = "", ylab = "", xaxt = 'n', yaxt = 'n',main = "Mastermind board game")
  nof_tries <- nof_try
  for (i in 1:nof_tries) { #rows
    for (j in 1:nof_col) { #columns
      col <- 50*(1:nof_col-1)
      rect(100+col[j], 500-(i*30), 150+col[j], 475-(i*30), col = 'white')
    }
  }
  colours = c('Red','Green','Blue','Yellow','Brown','Orange')
  for (z in 1:nof_col) {
    rect(100+z*50, 100, 150+z*50, 150, col = colours[z])
  } 
}


add_rect <- function(colour,try,nof_try=10) {
  par(new = TRUE)
  max_tries <- numberOfColors*nof_try #10 rows
  if (try %% numberOfColors == 0) { #number of tries = number of colours
    nof_try <- try/numberOfColors
    add_key_pegs(input_colours,store_secret, nof_try)
  } 
  
  if (try > numberOfColors){
    row <- ceiling((try/numberOfColors))
    rect_order <- abs(try-((row-1)*numberOfColors))
  } else {
    row <- 1
    rect_order <- try
  }
  rect(100+(rect_order*50)-50, 500-(row*30),150+((rect_order*50))-50, 475-(row*30), col = colour)
}


add_key_pegs <- function(input_colours, store_secret,nof_try){
  
  ss <- store_secret
  ic <- input_colours 
  ss1 <- as.vector(strsplit(as.character(ss), "")[[1]])
  ic1 <- as.vector(strsplit(as.character(ic), "")[[1]])
  
  
  white <- ""
  black <- ""
  for (i in 1:length(ss1)){
    for (j in 1:length(ic1)){
      if (i==j && ss1[i] == ic1[j]) {black <- as.integer(paste( c(black, ic1[j]),collapse=""))}
      if (ss1[i] == ic1[j]) { white <- as.integer(paste( c(white, ic1[j]),collapse="")) }
    }
  }
  
  black1 <- as.vector(strsplit(as.character(black), "")[[1]])
  white1 <- as.vector(strsplit(as.character(white), "")[[1]])
  black <- nchar(black)
  white <- length(unique(setdiff(white1, black1)))
  nof_tokes <- black + white
  tok <- replicate(black, "black")
  en <- replicate(white, "gray")
  token <- c(tok, en)
  name1 <- token[1]
  name2 <- token[2]
  name3 <- token[3]
  name4 <- token[4]
  
  if (nof_tokes == 1) {
    par(new = TRUE)
    plot(450,500-(nof_try*30), col = name1, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  }
  if (nof_tokes == 2) {
    par(new = TRUE)
    plot(450,500-(nof_try*30), col = name1, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(450,490-(nof_try*30), col = name2, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  }
  if (nof_tokes == 3) {
    par(new = TRUE)
    plot(450,500-(nof_try*30), col = name1, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(450,490-(nof_try*30), col = name2, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(470,500-(nof_try*30), col = name3, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  }
  if (nof_tokes == 4) {
    par(new = TRUE)
    plot(450,500-(nof_try*30), col = name1, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(450,490-(nof_try*30), col = name2, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(470,500-(nof_try*30), col = name3, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    par(new = TRUE)
    plot(470,490-(nof_try*30), col = name4, lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
  }
}

get_secret <- function(nof_col, colours_repeat=FALSE) {
  colours <- c(1:4)
  s <- sample(colours,nof_col, replace=colours_repeat) 
  s <- paste(s, collapse="")
  return(as.integer(s))
}


game <- function(numberOfColors=4, numberOfTries=10){
   rm(list = ls())
   x11()
   end_game <- 1
   count <<- 0
   get_board(nof_col = numberOfColors, nof_try = numberOfTries)
   store_secret <<- get_secret(nof_col=numberOfColors, colours_repeat =TRUE)
   input_colours <<- 0L
   nof_selection <- numberOfColors
   max_tries <- nof_selection*numberOfTries 
 
   while (end_game <= max_tries && store_secret != input_colours) {
     mouse.at <- locator(n = 1, type = "o") 
     x.at <- mouse.at$x
     y.at <- mouse.at$y
     
     
     if (x.at >= 150 & x.at < 200 & y.at >= 100 & y.at <=150) {
       input_colours <<- as.integer(paste( c(input_colours, 1),collapse=""))
       add_rect('Red',end_game) 
     }
     if (x.at >= 200 & x.at < 250 & y.at >= 100 & y.at <=150) {
       input_colours <<- as.integer(paste( c(input_colours, 2),collapse=""))
       add_rect('Green',end_game) 
     }
     if (x.at >= 250 & x.at < 300 & y.at >= 100 & y.at <=150) {
       input_colours <<- as.integer(paste( c(input_colours, 3),collapse=""))
       add_rect('Blue',end_game) 
     }
     if (x.at >= 300 & x.at < 350 & y.at >= 100 & y.at <=150) {
       input_colours <<- as.integer(paste( c(input_colours, 4),collapse=""))
       add_rect('Yellow',end_game) 
     } 

     #end game if needed 
     if (store_secret == input_colours) {
       par(new = TRUE)
       mtext("GAME WON", side=1)
       break
     }

      #end game if needed 
     if (end_game == max_tries) {
       par(new = TRUE)
       mtext("END GAME", side=1)
     }
     
    if (end_game %% numberOfColors == 0)  {
           add_key_pegs(input_colours, store_secret, count)
           input_colours <<- 0L 
           count <<- count + 1
      } 
     # increment next level
     end_game = end_game + 1      
   }
}
  
######################
### Start The game ###
######################

game()
  
