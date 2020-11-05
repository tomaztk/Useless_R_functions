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


WackyPassword <- function(WP_length){
  #charblock1 = c(176:178, 185: 188, 200:206)
  charblock1 <- c("\u2591","\u2592","\u2593")
  charblock2 = c(73,105,108,124,49,33)
  numberblock3 <- sample(0:9, length(5),replace = TRUE)
  
  pass = ""
  Encoding(pass) <- "UTF-8"
  
      while (nchar(pass) <= WP_length) {
        res2 <- sample(charblock2, 100,replace = TRUE)
        res2 <- rawToChar(as.raw(res2))
        Encoding(res2) <- "UTF-8"
       # cat(res2)
        pass <- paste0(pass,substr(res2,1,5),collapse="", sep= "")
        
        
        res1 <- sample(charblock1, 100,replace = TRUE)
        #res1 <- (rawToChar(as.raw(res1)))
        #xx <- iconv(res1, "raw", "UTF-8")
        #res1 <- (as.raw(res1))
        #enc2utf8(res1)
        Encoding(res1) <- "UTF-8"
        res <- paste0(res1, sep = "", collapse = "")
        # cat(res1)
        pass <- paste0(pass,substr(res,1,5), sep="", collapse = "")
          }     

  cat(eval(pass))
}



WackyPassword(18)


