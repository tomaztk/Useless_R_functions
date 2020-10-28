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
      print("true Fri-13")
  } else {
      print("naaah!")
    }
  }  

IsItFriday13()



# non-deterministic
IsItFullMoon <- function(){

# when date (conert to 113) is 14 -> is getting full, else empty, based on Hijri (Kuwait) Calendar
  
  
  // Use current UTC date and time for this demo
  date_default_timezone_set('UTC');
  $thedate = date('Y-m-d H:i:s');
  $unixdate = strtotime($thedate);
  
  // The duration in days of a lunar cycle
  $lunardays = 29.53058770576;
  // Seconds in lunar cycle
  $lunarsecs = $lunardays * (24 * 60 *60);
  // Date time of first new moon in year 2000
  $new2000 = strtotime("2000-01-06 18:14");   

}  

IsIfFullMoon()

