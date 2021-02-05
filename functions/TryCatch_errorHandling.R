##########################################
# 
# Playing with TryCatch the error
#
# Series:
# Little Useless-useful R functions #17
# Created: February 02, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################


sum <- function(a,b){
  #return(a+b)
  #print(a+b)
 rr <<-a+b
}



sum(2,3)
df_error <- data.frame(error = " ", timestamp = Sys.time() )


sum_error <- function(expr){
res_try  <- tryCatch(expr,
           error = function(e){
             #message("Vrni napako ", e)
             print(1)
             ee <<- e
             
           },
           warning = function(w){
             #message("Vrni sporočilo:", w)
             print(0)
           })
if (res_try != 1){
  print(0)
} else {
  df_error <- rbind(df_error, data.frame(error=ee$message, timestamp = Sys.time()))
}
           
}


# testing

sum_error(sum(2,"3"))

sum_error(sum(2,5))





