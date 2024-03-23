##########################################
# 
# Reverse Hello world
#
# Series:
# Little Useless-useful R functions #55
# Created: March 20, 2024
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


# reverse Hello World
hello_world <- function(print){
  if (print == "print"){
    print("Hello World")
  } else {
    cat("\rWell ...")
  }
  
}


# run reverse function :)

hello_world("print")
