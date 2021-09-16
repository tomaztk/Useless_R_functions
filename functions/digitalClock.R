##########################################
# 
# Annoying useless small digital clock
#
# Series:
# Little Useless-useful R functions #26
# Created: September 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


SmallDigitalTime <- function() {
  cat("\014")
  while(TRUE){
    Sys.sleep(0.1)
   cat("\r", strftime(Sys.time(), format="%H:%M:%S"))
  }
}

# Run function / clock
SmallDigitalTime()



