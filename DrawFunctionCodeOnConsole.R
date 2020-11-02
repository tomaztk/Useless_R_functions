##########################################
# 
# Draw function on console plot with code
# Series:
# Little Useless-useful R functions #2
# Created: November 1, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


# random text, later to be coode
tkt <- "to je tole, pa takole je, ker tako je in sicer ne bi tako bilo, ker tako je in pika. Tako gre, ker tako je in sicer ne more tako, ker je kot ni in pika."

r <- nchar(tkt)
s <- ceiling(sqrt(r))


draw_rect <- function(size, codetext) {
for (i in 1:size){
   print( paste0("#",substr(codetext,i*size+1, i*size+size),"#" ))
   }
 }

#test ....
draw_rect(15,tkt)




draw_shape <- function(fun, size, codetext){
  
  fun <- c("cos", "sin", "x", "x2", "x3", "xn")
  
  
}


