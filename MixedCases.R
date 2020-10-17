#################################
# 
# Random Small Caps Letter 
# Created: October 15, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
############################### 



MixedCases <- function(stavek) {
  nov_stavek = ""
  cifra = 1
  crka = ""
  is.upper <- "[A-Z]"
  is.lower <- "[a-z]"
  
  
  for (crka in strsplit(stavek, "")[[1]]) {
    if (nchar(nov_stavek)<=2) {
      random_stevilka = sample(0:1, 1, replace=TRUE)
      if (random_stevilka == 0) {
        nov_stavek = paste(nov_stavek,toupper(crka), sep = "")
      }
      else {
        nov_stavek = paste(nov_stavek,tolower(crka), sep = "")
      }
    }
    else {
      if (grepl(is.upper, strsplit(nov_stavek,"")[[1]][cifra-2]) & 
            grepl(is.upper, strsplit(nov_stavek,"")[[1]][cifra-1]) | 
            
            grepl(is.lower, strsplit(nov_stavek,"")[[1]][cifra-2]) & 
            grepl(is.lower, strsplit(nov_stavek,"")[[1]][cifra-1])) {
        if ( grepl(is.upper, strsplit(nov_stavek,"")[[1]][cifra-1]) ) {
          nov_stavek = paste(nov_stavek, tolower(crka), sep = "")
        }
        else {
          nov_stavek = paste(nov_stavek, toupper(crka), sep = "")
        }
      }
      else {
        random_stevilka = sample(0:1, 1, replace=TRUE)
        if (random_stevilka == 0) {
          nov_stavek = paste(nov_stavek, toupper(crka), sep = "") 
        }
        else { 
          nov_stavek = paste(nov_stavek, tolower(crka), sep = "") 
        }
      }
    }
    cifra = cifra + 1
  }
  return(nov_stavek)
}

set.seed(12358)
MixedCases("This is useless R Function that seems to exists.")
