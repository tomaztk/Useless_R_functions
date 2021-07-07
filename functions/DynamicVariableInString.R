##########################################
# 
# Variables in strings
#
# Series:
# Little Useless-useful R functions #25
# Created: July 2, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# 
# Changelog: 
###########################################

var <- "car"
a <- "This is a {var}"
# Run cat_v(a) or cat_v("This is a {var}")
# Result: "This is a car"


#cat with variables
cat_v(a)

cat_a <- function(tex){
  subs <- tex # inbetween { }
  t <- get(eval("var"))
  t
}


