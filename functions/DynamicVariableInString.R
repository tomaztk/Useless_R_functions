##########################################
# 
# Inserting variable values into strings
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

vv <- "tomaz"

#function
cat_v <- function(tex){
    rr <- " "
    pos_1 <- which(strsplit(tex, "")[[1]]=="{")
    pos_2 <- which(strsplit(tex, "")[[1]]=="}")
    end_pos <- nchar(tex)
    varname <- substr(tex, pos_1+1, pos_2-1)
    t <- get(eval(varname))
    t1 <- substr(tex, 1, pos_1-1)
    t2 <- substr(tex, pos_2+1, end_pos)
    t1 <- paste0(t1, t, t2)
    cat(t1)
}



#cat with variables
#cat_v(a)
cat_v("This is text by: {vv} and today is a great day!")
cat_v("This is text by: {vv}")


