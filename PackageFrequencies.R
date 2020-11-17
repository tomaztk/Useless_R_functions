##########################################
# 
# Frequency of same functions  in vignettes / libraries
# Series:
# Little Useless-useful R functions #8
# Created: November 11, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: - 
###########################################


funkyFun <- function(libnames){
     oldvalueWarning <- getOption("warn")
     options(warn = -1)
     libnames <<- installed.packages()[,1]
     libnames <<- installed.packages()[,1]
     df <- data.frame(packname = NULL, funkyName = c(NULL, NULL))
     for (i in 1:2){
        #for (i in 1:length(libnames)){
        com <- paste0("require(", libnames[i],")")
        eval(parse(text= com))
        str <- paste0("package:", libnames[i])
        funk <- (ls(str))
        da <- cbind(libnames[i], funk)
        df <- rbind(df, da)
     }  
     options(warn = oldvalueWarning)  
}



