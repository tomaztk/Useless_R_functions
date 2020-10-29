##########################################
# 
# Is if full moon yet?
#
# Series:
# Little Useless-useful R functions #4
# Created: October 29, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################



# non-deterministic
IsItFullMoon <- function(){
  
  #when date (conert to 113) is 14 -> is getting full, else empty, based on Hijri (Kuwait) Calendar
  #da <- as.Date('2020-10-31')
  da <- Sys.Date()
  julianConstant <- 2451549.5
  cycle <- 29.53
  y <- as.integer(format(da, format="%Y"))
  m <- as.integer(format(da, format="%m"))
  d <- as.integer(format(da, format="%d"))
  
  # If the month is January or February, subtract 1 from the year and add 12 to the month.
  if(m==1 | m==2) {
    y <- y-1
    m <- m + 12
  }
  
  a = y/100
  b = a/4
  c = 2-a+b
  e = 365.25 * (y + 4716)
  f = 30.6001 * (m+1)
  jd = c+d+e+f-1524.5
  
  
  
  do_nove_lune = jd - 2451549.5
  semi <- do_nove_lune/29.53
  dec <- semi%%1
  #dec*cycle
  if (dec*cycle>= 14.50 & dec*cycle <= 15.50){
    print("Evo ti!")
  } else {
    print("Še poèakaj")
  }
  
  
}  

IsItFullMoon()


