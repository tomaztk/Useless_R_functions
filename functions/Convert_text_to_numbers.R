
##########################################
#
#
# Old phone converted from text to numbers
# and from numbers to text
#
# Series:
# Little Useless-useful R functions #51
# Created: May 01, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################

#helper data
let <- c("a","b","c","",
         "d","e","f","",
         "g","h","i","",
         "j","k","l","",
         "m","n","o","",
         "p","q","r","s",
         "t","u","v","",
         "w","x","y","z")

mm <- matrix(let, nrow = 8, ncol=4, byrow = TRUE, 
             dimnames = list(
                  c("N2","N3","N4","N5","N6","N7","N8","N9"),
                  c("P.1","P.2","P.3","P.4"))
              )



# function test

SMSconverter("text")
SMSconverter("833998")
