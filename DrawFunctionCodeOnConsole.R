##########################################
# 
# Draw function on console plot with code
# Series:
# Little Useless-useful R functions #2
# Created: November 1, 2020 - work in prog
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


# random text, later to be coode
tkt <- "to je tole, pa takole je, ker tako je in sicer ne bi tako bilo, ker tako je in pika. Tako gre, ker tako je in sicer ne more tako, ker je kot ni in pika."



##### 1. Rectangle

draw_rect <- function(size, codetext) {
  r <- nchar(codetext)
  #s <- ceiling(sqrt(r))
  x = size
  y = floor(r/x)
  for (i in 1:y){
    if (i == 1){
      cat( paste0("#",substr(codetext,1, size),"#","\n" ))
    } else {
      cat( paste0("#",substr(codetext,1+(size*i), size+(size*i)),"#","\n" ))
    }
  }
}

#test
draw_rect(10,tkt)



##### 1. Parallelogram
draw_parallel <- function(h, codetext){
  r <- nchar(codetext)
  a = ceiling(sqrt(r*4/1.73))
  v = ceiling(r/a) + h 
  for (i in 1:v){
    if (i == 1){
      cat( paste0(paste0(replicate(i,"\n "), collapse=""),substr(codetext,1, a),paste0(replicate(i,"\n "), collapse="")))
    } else {
      cat( paste0(paste0(replicate(i," "), collapse=""),substr(codetext,1+(a*i)-i, a+(a*i)-i),paste0(replicate(i," "), collapse=""),"\n" ))
    }
  }
}


#test
draw_parallel(10,tkt)



draw_triang <- function(h, codetext){
  r <- nchar(codetext)
  a = ceiling(sqrt(r*4/1.73))
  v = ceiling(r/a) + h 
  for (i in 1:v){
    presledek = i*2
    cat( paste0(paste0(replicate(presledek/2, " "), collapse =""), substr(codetext,1,a-presledek), paste0(replicate(presledek/2, " "), collapse = ""), "\n"))   
  }
}

draw_triang(10, tkt)




draw_shape <- function(fun, size, codetext){
  
  fun <- c("cos", "sin", "x", "x2", "x3", "xn")
  
}


