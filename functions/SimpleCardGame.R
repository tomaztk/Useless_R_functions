##########################################
# 
# Two players play a stack of cards [numbers]
# 
# Series:
# Little Useless-useful R functions #12
# Created: January  01, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# Changelog: 
#  
###########################################

#Ground rules are simple:
#- both players get same number of cards
#- winner of the game is the player taking all the cards
#- both players draw one card at the time from the top of the stack
#- if both players draw the same card, both return the card at the bottom of the stack
#- player takes the draw if the number is higher than opponents'
#- cards are put at the stack in following order: first card is the card of the winner (own card) and second is card from opponent

library(ggplot2)


draw <- function(n1,n2){
  if (n1 > n2) {
    val <- c(n1, n2)
    p1 <<- append(p1,val)
    p1 <<- p1[(2:length(p1))]
    p2 <<- p2[(2:length(p2))]
  }  else if (n1 < n2) {
    val <- c(n2,n1)
    p2 <<- append(p2, val)
    p1 <<- p1[(2:length(p1))]
    p2 <<- p2[(2:length(p2))]
  } else {
    p1 <<- append(p1,n1)
    p2 <<- append(p2,n2)
    p1 <<- p1[(2:length(p1))]
    p2 <<- p2[(2:length(p2))]
  }
}

play <- function(){
  i = 1
  while(length(p1) != 0 | length(p2) != 0){
    print(paste0("Playing round: ",i, ". Player1 draws: ", p1[1], " while Player2 draws: ", p2[1]), collapse = NULL)
    draw(p1[1],p2[1])
    i = i + 1
    if (is.na(p1) == TRUE) {
      print(paste0("Player 2 Wins! in ", i, " round"), collapse = NULL)
      break()
    } else if (is.na(p2) == TRUE) {
      print(paste0("Player 1 Wins! in ", i, " round"), collapse = NULL)
      break()
    }
  }
}

playWithStats <- function(){
  i = 1
  df <- NULL
  df <- data.frame(round=1, p1=length(p1), p2=length(p2))
  while(length(p1) != 0 | length(p2) != 0){
    print(paste0("Playing round: ",i, ". Player1 draws: ", p1[1], " while Player2 draws: ", p2[1]), collapse = NULL)
    draw(p1[1],p2[1])
    i = i + 1
    df <- rbind(df, data.frame(round=i, p1=length(p1), p2=length(p2)))
    if (is.na(p1) == TRUE) {
      print(paste0("Player 2 Wins! in ", i, " round"), collapse = NULL)
      break()
    } else if (is.na(p2) == TRUE) {
      print(paste0("Player 1 Wins! in ", i, " round"), collapse = NULL)
      break()
    }
  }
  df_out <<- df
  g <- ggplot(df_out, aes(round))
  g <- g + geom_line(aes(y=p1), colour="red")
  g <- g + geom_line(aes(y=p2), colour="green")
  g <- g + xlab("Number of rounds") + ylab("Number of cards")
  g
  plot(g)
}


############################
### Play without stats
############################

set.seed(2908)
p1 <- c(4,6,5,7,3,6)
p2 <- c(6,2,1,7,8,7)
play()



############################
### Entering play with stats
############################

# never converges 
set.seed(2908)
p1 <- sample(1:10,10,replace=FALSE)
p2 <- sample(1:10,10,replace=FALSE)
playWithStats()


# never converges 
set.seed(2908)
p1 <- sample(1:6,6,replace=FALSE)
p2 <- sample(1:6,6,replace=FALSE)
playWithStats()


# never converges 
set.seed(2908)
p1 <- sample(1:5,5,replace=FALSE)
p2 <- sample(1:5,5,replace=FALSE)
rm(df_out)
playWithStats()


# never converges 
set.seed(2908)
p1 <- sample(1:4,4,replace=FALSE)
p2 <- sample(1:4,4,replace=FALSE)
playWithStats()

# never converges 
set.seed(2908)
p1 <- sample(1:3,3,replace=FALSE)
p2 <- sample(1:3,3,replace=FALSE)
playWithStats()


# converges in 229 rounds
set.seed(2908)
p1 <- sample(1:10,10,replace=TRUE)
p2 <- sample(1:10,10,replace=TRUE)
playWithStats()



# Why never converges  -> because it sorts all the cards
set.seed(2908)
p1 <- sample(1:3,3,replace=FALSE)
p2 <- sample(1:3,3,replace=FALSE)
playWithStats()

#3 steps with three cards
draw(2,2)
draw(1,3)
draw(3,1)


# Why never converges -> because it sorts all the cards
set.seed(2908)
p1 <- sample(1:4,4,replace=FALSE)
p2 <- sample(1:4,4,replace=FALSE)
playWithStats()


#8 steps with four cards
draw(2,4)
draw(3,3)
draw(1,1)
draw(4,2)
draw(3,4)
draw(1,2)
draw(4,3)
draw(2,1)
