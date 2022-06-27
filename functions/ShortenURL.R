

##########################################
# 
# Plotting QR code and URL Shortener
# 
# Series:
# Little Useless-useful R functions #40
# Created: June 28, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(jsonlite)
library(httr)


# Shorten URL
ShortenURL <- function(URL2Bshort, linkPreview = FALSE) {
  
  apiCall <- if(linkPreview) {"http://v.gd/create.php?format=json"} else {"http://is.gd/create.php?format=json"}
  URLQuery <- list(url = URL2Bshort)
  # get request
  request <- httr::GET(apiCall, query = URLQuery)
  Callcontent <- httr::content(request, as = "text", encoding = "utf-8")
  ShortURL <- jsonlite::fromJSON(Callcontent)
  
  return(ShortURL)
  
}


# test it!
ShortenURL("https://medium.com/@tomazkastrun/culture-fit-or-culture-add-e89ca0485ed1")
# https://is.gd/YU3c8m


# Create QR Code
