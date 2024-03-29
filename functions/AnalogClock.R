##########################################
# 
# Annoying useless analog clock
#
# Series:
# Little Useless-useful R functions #26
# Created: September 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

require(grid)
#install.packages("beepr")


DrawClock <- function(hour, minute, second) {
  
  t <- seq(0, 2*pi, length=13)[-13]
  x <- cos(t)
  y <- sin(t)

  
  grid.newpage()
  pushViewport(dataViewport(x, y, gp=gpar(lwd=3)))
  
  # Clock background
  grid.circle(x=0, y=0, default="native", r=unit(1, "native"))

  # Hour hand
  hourAngle <- pi/2 - (hour + minute/60)/12*2*pi
  grid.segments(0, 0, 0.6*cos(hourAngle), .6*sin(hourAngle), default="native", gp=gpar(lex=4, col="red"))
  
  # Minute hand
  minuteAngle <- pi/2 - (minute)/60*2*pi
  grid.segments(0, 0, 0.8*cos(minuteAngle), .8*sin(minuteAngle),default="native", gp=gpar(lex=2))    

  # Second hand
  secondAngle <- pi/2 - (second)/60*2*pi
  grid.segments(0, 0, 
          0.8*cos(secondAngle), .7*sin(secondAngle), default="native", gp=gpar(lex=1, col = "blue"), draw=TRUE)    
  grid.circle(0,0, default="native", r=unit(1, "mm"), gp=gpar(fill="white"))
}



AnalogClock <- function() {
    while(TRUE){
    hh <- as.integer(format(Sys.time(), format="%H"))
    mm <- as.integer(format(Sys.time(), format="%M"))
    ss <- as.integer(format(Sys.time(), format="%S"))
    Sys.sleep(1)
    DrawClock(hh,mm,ss)
    beepr::beep(sound = 1, expr = NULL)
    }
}

#Run Function / clock
AnalogClock()





