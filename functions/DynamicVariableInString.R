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

name_v <- "tomaz"
a <- "This is text with: [vv]"
tex <- "This is text with: [vv]"

# Run cat_v(a) or cat_v("This is text with variable value: {var}")
# Result: "This is text with variable value: car"



cat_v <- function(tex){
    pos_1 <- which(strsplit(tex, "")[[1]]=="[")
    pos_2 <- which(strsplit(tex, "")[[1]]=="]")
    varname <- substr(tex, pos_1+1, pos_2-1)
    t <- get(eval(varname))
    rr <- sub(varname, t, tex)
    cat(rr)
}


#cat with variables
#cat_v(a)
cat_v("This is text with: [name_v]")


