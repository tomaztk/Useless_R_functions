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
a <- "This is text with  value: [var]"
# Run cat_v(a) or cat_v("This is text with variable value: {var}")
# Result: "This is text with variable value: car"


#cat with variables
#cat_v(a)
cat_v("This is text with  value: [var]")

cat_v <- function(tex){
    pos_1 <- which(strsplit(a, "")[[1]]=="[")
    pos_2 <- which(strsplit(a, "")[[1]]=="]")
    varname <- substr(a, pos_1, pos_2)
    t <- get(eval("var"))
    rr <- sub(varname, t, a)
    cat(rr)
}




