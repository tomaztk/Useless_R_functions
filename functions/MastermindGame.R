
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


get_secret <- function(nof_col, colours_repeat=FALSE) {
    colours <- c('Red','Green','Blue','Yellow','Brown','Orange')
    sample(colours,nof_col, replace=colours_repeat)
}

# store secret
store_secret <- get_secret(nof_col=numberOfColors, colours_repeat =TRUE)


get_board <- function(nof_col, nof_pegs, nof_try=10){
  plot.new()
  op <- par(bg = "white")
  grid(nx = 6, ny = 12, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  
  # adding boarders 
  par(new = TRUE)
  plot(c(100,500), c(100,500), xlab = "", ylab = "",  xaxt = 'n', yaxt = 'n',main = "Mastermind board game")
  
  nof_tries <- nof_try
  
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


add_rect <- function(colour,try) {
  par(new = TRUE)
  max_tries <- numberOfColors*10 #10 rows
  if (try %% numberOfColors  == 0) {print(inputted_colours) } #print("Check Key Pegs!")}

  if (try > numberOfColors){
    row <- ceiling((try/numberOfColors))
    rect_order <- abs(try-((row-1)*numberOfColors))
  } else {
    row <- 1
    rect_order <- try
  }
  #print(try)
  print(row)
  print(rect_order)
  rect(100+(rect_order*50)-50, 500-(row*30),150+((rect_order*50))-50, 475-(row*30), col = colour)
}

check_key_Pegs <- function(input_colours, store_secret){

  ss <- store_secret
  ic <- inputted_colours
  
  black <- length(which(ss==ic))
  white <- length(which((ic %in% ss)==TRUE)) - black
  
  for (h in 0:black) {
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
  
  for (h in 0:white) {
    if (h == 0) {
      par(new = TRUE)
      plot(450,500-(i*30), col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    }
    if (h == 1) {
      par(new = TRUE)
      plot(450,490-(i*30), col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    }
    if (h == 2) {
      par(new = TRUE)
      plot(470,500-(i*30), col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    }
    if (h == 3) {
      par(new = TRUE)
      plot(470,490-(i*30), col = "gray", lwd = 2, xaxt = 'n', yaxt = 'n', xlab = "",ylab = "",xlim=range(100:500), ylim=range(100:500))
    }
  }
  
  
}


# test with x11()
game <- function(){
  won <- FALSE
  end_game <- 1
  
  # x11()
  plot.new()
  get_board(nof_col=6,nof_pegs=4,nof_try=10)
 
   #colours selection
  colours = c('Red','Green','Blue','Yellow','Brown','Orange')
  for (z in 1:6) {
    rect(100+z*50, 100, 150+z*50, 150, col = colours[z])
    print(100+z*50)
  }
  #select 6
  nof_selection <- 6 # nof_col
  max_tries <- nof_selection*2 # nof_try only for test 10
  while (end_game <= max_tries & won == FALSE) {
        for (i in 1:max_tries) {
          mouse.at <- locator(n = 1, type = "o") 
          x.at <<- mouse.at$x
          y.at <<- mouse.at$y
          inputted_colours <<- NULL
          if (x.at >= 150 & x.at < 200 & y.at >= 100 & y.at <=150) {
            #print('Red')
            inputted_colours <<- c(inputted_colours, 'Red')
            add_rect('Red',i) 
            }
          if (x.at >= 200 & x.at < 250 & y.at >= 100 & y.at <=150) {
            #print('Green')
            inputted_colours <<- c(inputted_colours, 'Green')
            add_rect('Green',i) 
            }
          if (x.at >= 250 & x.at < 300 & y.at >= 100 & y.at <=150) {
            #print('Blue')
            inputted_colours <<- c(inputted_colours, 'Blue')
            add_rect('Blue',i) 
            }
          if (x.at >= 300 & x.at < 350 & y.at >= 100 & y.at <=150) {
            #print('Yellow')
            inputted_colours <<- c(inputted_colours, 'Yellow')
            add_rect('Yellow',i) 
            }
          if (x.at >= 350 & x.at < 400 & y.at >= 100 & y.at <=150) {
            #print('Brown')
            inputted_colours <<- c(inputted_colours, 'Brown')
            add_rect('Brown',i) 
            }
          if (x.at >= 400 & x.at < 450 & y.at >= 100 & y.at <=150) {
            #print('Orange')
            inputted_colours <<- c(inputted_colours, 'Orange')
            add_rect('Orange',i) 
          }
        if (end_game %% 6 == 0){
          
          #print(inputted_colours)
          check_key_Pegs(input_colours = inputted_colours, store_secret = store_secret)
          if (store_secret == inputted_colours) {
            print("Game Won")
            break
          }
          }
        }
    graphics.off()
  }
  end_game <- end_game + 1
  print(end_game)
  
}


#Start The game
game()



# Rewrite:

# test with x11()
numberOfPegs <- 4
numberOfColors <- 4
numberOfTries <- 10


game <- function(numberOfColors=4, numberOfTries=10){
  won <- FALSE
  end_game <- 1
  
  get_secret <- function(nof_col, colours_repeat=FALSE) {
    colours <- c('Red','Green','Blue','Yellow') #,'Brown','Orange')
    sample(colours,nof_col, replace=colours_repeat)
  }
  
  
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
        # 6 per row | every second row empyt | start top - down
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
    
    black <- length(which(ss==ic))
    white <- abs(length(which(ic %in% ss)==TRUE) - black) 
    #nof_try <- 1
    nof_tokes <- black + white
    tok <- replicate(black, "black")
    en <- replicate(white, "white")
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
  
  # store secret
  store_secret <- get_secret(nof_col=numberOfColors, colours_repeat =TRUE)
  #input_colours <- character(length = numberOfColors)
  input_colours <- NULL
  #input_colours <- c("Blue", "Yellow","Green","Yellow") #, "Brown", "Green")
  get_board(nof_col = numberOfColors, nof_try = numberOfTries)
  
  nof_selection <- numberOfColors
  max_tries <- nof_selection*numberOfTries 
  
  while (end_game <= max_tries & won == FALSE) {
    for (i in 1:max_tries) {
      if (end_game %% numberOfColors != 0) {
        
        mouse.at <- locator(n = 1, type = "o") 
        x.at <<- mouse.at$x
        y.at <<- mouse.at$y
        if (x.at >= 150 & x.at < 200 & y.at >= 100 & y.at <=150) {
          input_colours <- c(input_colours, 'Red')
          add_rect('Red',i) 
        }
        if (x.at >= 200 & x.at < 250 & y.at >= 100 & y.at <=150) {
          input_colours <- c(input_colours, 'Green')
          add_rect('Green',i) 
        }
        if (x.at >= 250 & x.at < 300 & y.at >= 100 & y.at <=150) {
          input_colours <- c(input_colours, 'Blue')
          add_rect('Blue',i) 
        }
        if (x.at >= 300 & x.at < 350 & y.at >= 100 & y.at <=150) {
          input_colours <- c(input_colours, 'Yellow')
          add_rect('Yellow',i) 
        }
      } else {
        add_key_pegs(input_colours=input_colours, store_secret=store_secret,nof_try=10){
          
          if (store_secret == input_colours) {
            print("Game Won")
            break
          }
          input_colours <- character(length = numberOfColors)
        }
      }
      end_game <- end_game + 1
      
    }
    graphics.off()
  }
  
  
  
  #Start The game
  game()
  