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
#        - adding x11()
###########################################


#################################### 
##### Input bet as a function  #####
####################################


play_RPS <- function(bet) {
  bets <- c("R","P", "S") 
  if(bet %in% bets){
     
  solution_df <- data.frame(combo=c("RP", "PR", "PS", "SP", "RS", "SR", "PP", "RR", "SS"), win = c("01","10", "01","10", "10", "01", "00","00","00") )
  REngine <- sample(bets,1)  
  combo <- paste0(REngine,bet, collapse="")
  res <-solution_df[ which(solution_df$combo==combo),2]
  if (res=="10"){
    res_print <<- print(paste0("You lost. Computer draw: ", REngine), collapse="")
  } else if(res=="00"){
    res_print <<-print(paste0("It's a tie! Computer draw: ", REngine), collapse="")
    }else {
    res_print <<- print(paste0("You win! Computer draw: ", REngine), collapse="")
    }
   }
  else {
    print("Please input valid bet!")
  }
}

# Run test
play_RPS("R")


##############################
##### Using x11  ############
##############################


### Navigating through x11 with play_RPS function
## Concept/part of code of using x11() function by Darren Tsai (National Taipei University)
click <- function(rock.paper.scissors=defaultRPS){
  
  while(length(place.na)==9){
    mouse.at <- locator(n = 1, type = "p") 
    x.at <- round(mouse.at$x)
    y.at <- round(mouse.at$y)
    if(all(is.na(place.na))){
      defaultRPS <<- rock.paper.scissors()
    }else if(x.at > 3.5 | x.at < 0.5 | y.at > 3.5 | y.at < 0.5){
      r <<- r + 1
      title(sub=list("Click outside:Quit/inside:Restart", col="black", font=2, cex=2), line=2)
      if(r==2){
        dev.off()
        break
      }
    }else{
      if(r==1){
        ttt <<- rock.paper.scissors()
      }else{
        if(x.at==1){ res_print <<- play_RPS("R") 
          title(sub=list(res_print, col="black", font=0.5, cex=2.5), line=2)}
        if(x.at==2){ play_RPS("S") 
          title(sub=list(res_print, col="black", font=0.5, cex=2.5), line=2)}
        if(x.at==3){ play_RPS("P") 
          title(sub=list(res_print, col="black", font=0.5, cex=2.5), line=2)}
      }
    }
  }
}


#### Board 
rock.paper.scissors <- function(){
  place.na <<- matrix(1:9, 3, 3)
  value <<- matrix(-3, 3, 3)
  k <<- 1 ; r <<- 0
  image(1:3, 1:3, matrix(1:9, 3, 3), asp=c(1, 1), xaxt="n", yaxt="n", xlab="", ylab="", frame=FALSE, col=c("lightgreen", "lightYellow", "orchid1","lightgreen", "lightYellow", "orchid1","lightgreen", "lightYellow", "orchid1"))
  mtext(side=1, line=-10, at=1.0, adj=0, cex=0.7, 'Rock            Paper                  Scissors')
  }

### Start with x11 
start_game <- function(){
  x11()
  defaultRPS <<- rock.paper.scissors()
  click()
}

########################
#### Start the game ####
########################

start_game()

 