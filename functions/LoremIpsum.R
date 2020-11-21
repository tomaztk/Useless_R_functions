##########################################
# 
# Lorem Ipsum generator for R
# Series:
# Little Useless-useful R functions #6
# Created: October 31, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
# - improvements: count words for correct
#                 length
###########################################


function_list <- function(){
  
  lw <- builtins() #(internal = FALSE)
  lw2 <- help(package="base") 
  lw3 <- ls("package:base")
  lwA <- c(lw,lw2,lw3)
  lwA <- unique(lwA)
  lwA <- trimws(gsub("[[:punct:]]", " ", lwA))
  #ltrim / rtrim
  return(lwA)
  }



RLoremIpsum <- function(text_length, approx=TRUE){
  
  lw <- function_list()
  LorIps <- ''
  while (nchar(LorIps) < text_length) {
    lw <- gsub("^ *|(?<= ) | *$", "", lw, perl = TRUE)
    new_w <-  sample(lw,1, replace=TRUE)
    LorIps <- paste(LorIps, new_w, sep = " ")
    if (approx==FALSE){
    LorIps <- substr(LorIps, 1, text_length)
    }
  }
  
  #check last word - if cut, replace the last word (from last spacebar)
  last_word <- tail(strsplit(LorIps ,split=" ")[[1]],1)
  #if ((last_word %in% lw) == FALSE) {
    if ((nchar(last_word) == 1) == TRUE) {
    LorIps <- substr(LorIps, 1, nchar(LorIps)-1) # replace last char with blank space
    }
   #len <- nchar(last_word)
   #sapply(strsplit(lw, ","), nchar)  #== len
  #}
  return(LorIps)
}



# generated Lorem Ipsum with 1000 characters
RLoremIpsum(10000, approx=TRUE)

