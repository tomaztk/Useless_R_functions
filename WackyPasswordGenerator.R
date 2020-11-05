##########################################
# 
# Generate wacky (strong) password
# Series:
# Little Useless-useful R functions #8
# Created: November 5, 2020 - work in prog
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


WackyPassword <- function(WP_length, WP_level=TRUE){

  WP_length <- 20
  charblock1 = c(176:178, 185: 188, 200:206)
  charblock2 = c(73,105,108,124,49,33)
  numberblock3 <- sample(0:9, length(5),replace = TRUE)
  

   if (WP_level==TRUE){
 
     
     stopifnot(is.numeric(WP_length),  WP_length   > 0L,
               is.character(charblock1), charblock1  > 0L, charblock1 <= WP_length,
               is.numeric(charblock2), charblock2  > 0L, charblock2 <= WP_length,
               is.numeric(numberblock3), numberblock3  > 0L, numberblock3 <= WP_length,
               length(intersect(charblock1, charblock2, numberblock3)) == 0L)
        
     wacky <- charblock1
     pass[capitals] <- uc
     pass[numbers]  <- num
     
     paste0(pass, collapse = "")
     
  }
  if (WP_level==FALSE){
    
    rerw
  }
  
  data.frame(coderange,as.raw(coderange),row.names=rawToChar(as.raw(coderange),multiple=TRUE))
  
}



