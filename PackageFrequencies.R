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


funkyFun <- function(){
     libnames <- installed.packages()[,1]
     #exclude packages: 
     libnames <- libnames[ !(libnames %in% c("rJava","RPostgreSQL","XLConnect","xlsx","xlsxjars")) ]
     df <- data.frame(packname = NULL, funkyName = c(NULL,NULL))
     #for (i in 1:50){
     for (i in 1:length(libnames)){
        com <- paste0("require(", libnames[i],")")
        eval(parse(text= com))
        str <- paste0("package:", libnames[i])
        funk <- (ls(str))
        if (length(funk)==0){
          funk <- ifelse((length(funk)==0)==TRUE, "funkyFun", funk)
        }
        da <- cbind(libnames[i], funk)
        df <- rbind(df, da)
    
     }  
     no_freq <- data.frame(table(df$funk))
     all_duplicated_functions  <- no_freq[no_freq$Freq > 1,]
     all_duplicated_functions_per_package <- df[df$funk %in% no_freq$Var1[no_freq$Freq > 1],]
     all_duplicated_functions_per_package2 <<- df[df$funk %in% no_freq$Var1[no_freq$Freq > 1],]
     return(all_duplicated_functions_per_package)
}

########################################
##### run with warnings seen
##### Otherwise uncomment next 3 lines
########################################

#oldvalueWarning <- getOption("warn")
#options(warn = -1)
 funkyFun()
#options(warn = oldvalueWarning)  

