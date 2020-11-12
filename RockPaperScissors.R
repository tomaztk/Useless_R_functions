##########################################
# 
# Play Rock-Paper-Scissors with R
# Series:
# Little Useless-useful R functions #8
# Created: November 11, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - adding rocks
###########################################




##### Input bet as a function
play_RPS <- function(bet) {
   if(bet %in% c("R","P", "S")){
     
  solution_df <- data.frame(combo=c("RP", "PR", "PS", "SP", "RS", "SR", "PP", "RR", "SS"), win = c("01","10", "01","10", "10", "01", "00","00","00") )
  REngine <- sample(bets,1)  
  combo <- paste0(REngine,me, collapse="")
  res <-solution_df[ which(solution_df$combo==combo),2]
  if (res=="10"){
    print("You lose")
    print(paste0("Computer draw: ", REngine), collapse="")
  } else if(res=="00"){
    print("It's a tie")
    print(paste0("Computer draw: ", REngine), collapse="")
    }else {
    print("You win!")
    print(paste0("Computer draw: ", REngine), collapse="")
    }
   }
  else {
    print("Please input valid bet")
  }
}


play_RPS("R")

 