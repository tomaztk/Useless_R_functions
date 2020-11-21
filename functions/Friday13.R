##########################################
# 
# Is if full moon yet?
# Is it Friday 13th?
# Series:
# Little Useless-useful R functions #4
# Created: October 26, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


# non-deterministic
IsItFriday13 <- function(){
  #da <- "2020-11-13"
  da <- Sys.Date()
  rn <- as.POSIXlt(da)$wday
  d <- as.POSIXlt(da)$mday 
  
  if (rn == 5 & d == 13){
      print("It is a Friday the 13th!")
  } else {
      print("Not a Friday 13th!")
    }
  }  

IsItFriday13()

