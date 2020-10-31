##########################################
# 
# Lorem Ipsum generator for R
# Series:
# Little Useless-useful R functions #6
# Created: October 30, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
# - improvements: count words for correct
#                 length
###########################################


function_list <- function(){
  
  lw <- builtins() #(internal = FALSE)
  #lw2 <- help(package="base") 
  lw3 <- ls("package:base")
  lwA <- c(lw,lw3)
  lwA <- unique(lwA)
  lwA <- trimws(gsub("[[:punct:]]", " ", lwA))
  #ltrim / rtrim

  return(lwA)
  }


#function_list()


RLoremIpsum <- function(text_length){
  
  lw <- function_list()
  text_length <- 1000
  a <- do.call(paste0, replicate(1, sample(lw, text_length, TRUE), FALSE))
  a <- paste0(" ", a, sep = " ", collapse = "")
  a <- gsub("^ *|(?<= ) | *$", "", a, perl = TRUE)

  #check last word - if cut, replace the last word (from last spacebar)
  a <- substr(a, 1, text_length)
  return(a)
}


# generated Lorem Ipsum with 1000 characters
RLoremIpsum(1000)

a


last_word <- tail(strsplit(a ,split=" ")[[1]],1)
if last_word exist in lw (exit)
if (nchar(last_word) == 1) { gsub(a, tail(strsplit(a ,split=" ")[[1]],1), " ") } #if last word je 1 znak, replace s presledkom
if (nchar(last_word) <> 1) { ll <- nchar(last_word); re.findall(r'\b[a-zA-Z]{4}\b', lw)  } # je razrezana beseda, ki jo zamenjaš z drugo iz nabora


lw
length(gregexpr("\\W+", lw)) + 1 
