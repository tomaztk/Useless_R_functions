

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

library(qrcode)
library(tidyverse)
library(ggplot2)

text <- "https://medium.com/@tomazkastrun/culture-fit-or-culture-add-e89ca0485ed1"
color <- "green"
x <- qrcode_gen(text, plotQRcode=F, dataOutput=T)
x <- as.data.frame(x)

y = x
y$id <- rownames(y)

y <- gather(y, "key", "value", colnames(y)[-ncol(y)])

# y$key = factor(y$key, levels=rev(colnames(x)))
# y$id = factor(y$id, levels=rev(rownames(x)))

ggplot(y, aes(x=id, y=key)) + geom_tile(aes(fill=value), alpha=alpha)

## Test 2
x <- qr_code(text, ecl="M")
x <- as.data.frame(x)

y = x
y$id <- rownames(y)

y <- gather(y, "key", "value", colnames(y)[-ncol(y)])

ggplot(y, aes(x=id, y=key)) + geom_tile(aes(fill=value), alpha=alpha) # + scale_fill_gradient(low=White, high = color) + theme_void() 
                                           