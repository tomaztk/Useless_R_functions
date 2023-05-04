
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
let <- c("a","b","c","","d","e","f","","g","h","i","","j","k","l","","m","n","o","","p","q","r","s","t","u","v","","w","x","y","z")
mm <- matrix(let,  nrow = 8, ncol=4, byrow = TRUE, dimnames = list(
                                                            c("N2","N3","N4","N5","N6","N7","N8","N9"),
                                                            c("P.1","P.2","P.3","P.4")))



SMSconverter <- function(tt){
  st <- NULL
  # check if input string are letters
  if (!grepl("[^A-Za-z]", tt) == TRUE){
    for (i in 1:nchar(tt)){
      lt <- substr(tt,i,i)
      if (lt != " "){
        rn <- substr(rownames(which(mm == lt, arr.ind = T)),2,2)
        rep <- which(mm == lt, arr.ind = T)[2]
        st <- c(st, replicate(rep, rn))
      } else {
        st <- c(st, "0")
      }
    }
    #print(st)
  }
  
  # check if input string are numbers
  #if (!grepl("\\D", tt) == TRUE){
  if (grepl("[^A-Za-z]", tt) == TRUE){
    for (i in 1:length(tt)){
      ena <- tt[i]
      dva <- tt[i+1]
      if (ena != dva & !is.na(dva) & ena != " "){
        num <- substr(rownames(which(mm == ena, arr.ind = T)),2,2)
        times_num  <- which(mm == ena, arr.ind = T)[2]
        st <- c(st, replicate(times_num, num))
      }
      if (ena == dva & !is.na(dva) & ena != " "){
        num <- substr(rownames(which(mm == dva, arr.ind = T)),2,2)
        times_num  <- which(mm == dva, arr.ind = T)[2]
        st <- c(st, replicate(times_num, num))
      }
      if (ena == " "){
        st <- c(st, "9")
      }
     # print(st)
    }
  }
  print(paste0(st, collapse=""))
}



SMSconverter("hello")
SMSconverter("4433555555666")


# test
text = "hell oo "
brd <- unlist(strsplit(text, ""))
st <- NULL

for (i in 1:length(brd)){
  ena <- brd[i]
  dva <- brd[i+1]
  if (ena != dva & !is.na(dva) & ena != " "){
    num <- substr(rownames(which(mm == ena, arr.ind = T)),2,2)
    times_num  <- which(mm == ena, arr.ind = T)[2]
    st <- c(st, replicate(times_num, num))
  }
  if (ena == dva & !is.na(dva) & ena != " "){
    num <- substr(rownames(which(mm == dva, arr.ind = T)),2,2)
    times_num  <- which(mm == dva, arr.ind = T)[2]
    st <- c(st, replicate(times_num, num))
  }
  if (ena == " "){
    st <- c(st, "9")
  }
  #print(st)
}


paste0(st, collapse="")
