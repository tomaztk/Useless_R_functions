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

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Running on Linux/MacOS
WackyPassword <- function(WP_length){
  #charblock1 = c(176:178, 185: 188, 200:206)
  charblock1 <- c("\u2591","\u2592","\u2593")
  charblock2 = c(73,105,108,124,49,33)
  numberblock3 <- sample(0:9, length(5),replace = TRUE)
  
  pass = ""
  Encoding(pass) <- "UTF-8"
  ran2 <- floor(sample(1:WP_length/2))
  ran1 <- floor(sample(1:WP_length/2))
      while (nchar(pass) <= WP_length) {
        res2 <- sample(charblock2, 100,replace = TRUE)
        res2 <- rawToChar(as.raw(res2))
        Encoding(res2) <- "UTF-8"
        start2 <- sample(1:90,1)
        pass <- paste0(pass,substr(res2,start2,start2+ran2),collapse="", sep= "")
        
        
        res1 <- sample(charblock1, 100,replace = TRUE)
        Encoding(res1) <- "UTF-8"
        start1 <- sample(20:70,1)
        res <- paste0(res1, sep = "", collapse = "")
        pass <- paste0(pass,substr(res,start1,start1+ran1), sep="", collapse = "")
          }     

  cat(eval(substr(pass,1,WP_length)))
}



WackyPassword(18)


