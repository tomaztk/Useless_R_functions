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
  is.upper <- "[A-Z]"
  is.lower <- "[a-z]"
  
  chars <- strsplit(stavek, "")[[1]]
  for (i in seq_along(chars)) {
    if (i<=2) {
      random_stevilka = sample(0:1, 1, replace=TRUE)
      if (random_stevilka == 0) {
        chars[[i]] <- toupper(chars[[i]])
      }
      else {
        chars[[i]] <- tolower(chars[[i]])
      }
    }
    else {
      # if previous 2 characters have the same case, use the opposite 
      if (all(grepl(is.upper, chars[i-seq_len(2)]))) {
        chars[[i]] <- tolower(chars[[i]])
      }
      else if (all(grepl(is.lower, chars[i-seq_len(2)]))) {
        chars[[i]] <- toupper(chars[[i]])
      }
      else {
        random_stevilka = sample(0:1, 1, replace=TRUE)
        if (random_stevilka == 0) {
          chars[[i]] <- toupper(chars[[i]])
        }
        else { 
          chars[[i]] <- tolower(chars[[i]])
        }
      }
    }
  }
  return(paste(chars, collapse = ""))
}

set.seed(12358)
MixedCases("This is useless R Function that seems to exists.")
