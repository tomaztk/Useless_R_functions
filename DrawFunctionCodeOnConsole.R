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



tkt <- "to je tole, pa takole je, ker tako je in sicer ne bi tako bilo, ker tako je in pika. Tako gre, ker tako je in sicer ne more tako, ker je kot ni in pika."

r <- nchar(tkt)
s <- ceiling(sqrt(r))

bl <- "#############"

draw_rect <- funtion(i,i)
for (i in 1:s){
   print( paste0("#",substr(tkt,i*s+1, i*s+s),"#" ))
}
