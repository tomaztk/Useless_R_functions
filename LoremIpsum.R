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
  a <- do.call(paste0, replicate(1, sample(lw, text_length, TRUE), FALSE))
  a <- paste0(" ", a, sep = " ", collapse = "")
  a <- gsub("^ *|(?<= ) | *$", "", a, perl = TRUE)
  a <- substr(a, 1, text_length)
  return(a)
}


# generated Lorem Ipsum with 1000 characters
RLoremIpsum(1000)
