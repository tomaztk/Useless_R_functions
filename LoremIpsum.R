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
  gsub("[[:punct:]]", " ", lwA)
  return(lwA)
  }

function_list()


RLoremIpsum <- function(text_length){
  
  lw <- function_list()
  
}




