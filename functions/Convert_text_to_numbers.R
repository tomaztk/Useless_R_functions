
##########################################
#
#
# Old phone converted from text to numbers
# and from numbers to text
#
# Series:
# Little Useless-useful R functions #51
# Created: May 01, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################

#helper data
let <- c("a","b","c","",
         "d","e","f","",
         "g","h","i","",
         "j","k","l","",
         "m","n","o","",
         "p","q","r","s",
         "t","u","v","",
         "w","x","y","z")

mm <- matrix(let, nrow = 8, ncol=4, byrow = TRUE, 
             dimnames = list(
                  c("N2","N3","N4","N5","N6","N7","N8","N9"),
                  c("P.1","P.2","P.3","P.4"))
              )


SMSconverter <- function(tt){
  st <- NULL
  tti <- unlist(strsplit(paste0(tt, " "), ""))
  
  # check if input string are letters
  if (!grepl("[^A-Za-z]", tti[1]) == TRUE){
    for (i in 1:nchar(tt)){
      lt <- substr(tt,i,i)
      if (lt != " "){
        rn <- substr(rownames(which(mm == lt, arr.ind = T)),2,2)
        rep <- which(mm == lt, arr.ind = T)[2]
        st <- c(st, replicate(rep, rn))
      } else { st <- c(st, "0") }
    }
    print(paste0(st, collapse = ""))
  }
  
  # check if input string are numbers
  if(!grepl("\\D", tti[1]) == TRUE){
    tti <- unlist(strsplit(paste0(tt, ""), ""))
    st <- NULL
    tti <- unlist(strsplit(as.character(tt), ""))
    tmp <- rle(tti)
    for (i in 1:length(tmp$lengths)){
      rpt <- tmp$lengths[i]
      row_cnt <- tmp$values[i]
      lt <- mm[as.integer(row_cnt)-1,rpt]
      #spacebar
      
      st <- c(st, lt)  
    }
    
    print(paste(st, collapse=""))
  }

}

# function test

SMSconverter("text")
SMSconverter("833998")
