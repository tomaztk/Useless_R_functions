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



##### Input bet as a function
play_RPS <- function(bet) {
  bets <- c("R","P", "S") 
  if(bet %in% bets){
     
  solution_df <- data.frame(combo=c("RP", "PR", "PS", "SP", "RS", "SR", "PP", "RR", "SS"), win = c("01","10", "01","10", "10", "01", "00","00","00") )
  REngine <- sample(bets,1)  
  combo <- paste0(REngine,bet, collapse="")
  res <-solution_df[ which(solution_df$combo==combo),2]
  if (res=="10"){
    print(paste0("You lost. Computer draw: ", REngine), collapse="")
  } else if(res=="00"){
    print(paste0("It's a tie! Computer draw: ", REngine), collapse="")
    }else {
    print(paste0("You win! Computer draw: ", REngine), collapse="")
    }
   }
  else {
    print("Please input valid bet!")
  }
}


play_RPS("R")



##############################
##### Using x11  ############
##############################


click <- function(rock.paper.scissors=ttt){

  while(length(place.na)==3){
    mouse.at <- locator(n = 1, type = "p") 
    cat(mouse.at$x,"\t",  mouse.at$y, "\n")
    x.at <- round(mouse.at$x)
    y.at <- round(mouse.at$y)
    #cat(x.at,"\t",  y.at, "\n")
    if(all(is.na(place.na))){
      ttt <<- rock.paper.scissors()
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
        #aim(x.at, y.at)
        #place.na 1,2,3: if 1 = R, 2 = P, 3 = S
        play_RPS("R")
        
      }
    }
  }
}


########################
# RPS Function #
########################

rock.paper.scissors <- function(){
  
  place.na <<- matrix(1:3, 1, 3)
  x <- seq(1:3)
  y <- 1
  image(x=x, y=y , z=outer(x,y), asp=c(1, 3), xaxt="n", yaxt="n", xlab="", ylab="", frame=F, col=c("lightgreen", "lightYellow", "orchid1"))
  
}


start_game <- function(){
  x11()
  ttt <<- rock.paper.scissors()
  click()
}

0.5, 2.34
0.5, -0.34
1.48  2.34
1.49  -0.34

start_game()


dev.off()



 