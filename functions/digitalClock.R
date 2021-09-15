##########################################
# 
# Annoying useless digital clock
#
# Series:
# Little Useless-useful R functions #26
# Created: September 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


DigitalTime <- function() {
  cat(("\014"))
   while(TRUE){
    hh <- (format(Sys.time(), format="%H"))
    mm <- (format(Sys.time(), format="%M"))
    ss <- (format(Sys.time(), format="%S"))
    Sys.sleep(1)

    time <- sprintf('%s', paste0(hh, ":", mm, ":", ss) )
    cat("\r", time)
   }
}

DigitalTime()



