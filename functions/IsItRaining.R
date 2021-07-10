##########################################
# 
# Is it raining?
# With help of openweathermapAPI
#
# Series:
# Little Useless-useful R functions #25
# Created: June 20, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# 
# Changelog: 
###########################################

library(jsonlite)


isItRainingYet <- function(city){
  
  #get api 
  #Ljubljana, Slovenija
  API_key = "5xxxxeexxxxxcbyyyy8xxxxxexxxxxxa1"
  City_name = city #"Ljubljana"
  
  api <- paste0("http://api.openweathermap.org/data/2.5/weather?q=",City_name,"&appid=",API_key)
  res <- fromJSON(api)
  perc <- res$clouds$all[1]
  perc <- as.integer(perc)
  
  rain <- " "
  if (perc<=20) rain <-'Meeh'
  if (perc>20 & perc<=60) rain <- 'Huuuh, but still meeh'
  if (perc>60 & perc<=85) rain <- 'Looking better'
  if (perc>85) rain <- 'Bring it on!'


  return(rain)
    
}


isItRainingYet("Ljubljana")

