#################################
# 
# Random Small Caps Letter 
# Created: October 15, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
############################### 



MixedCases <- function(string) {
  is.upper <- "[A-Z]"
  is.lower <- "[a-z]"
  
  chars <- strsplit(string, "")[[1]]
  for (i in seq_along(chars)) {
    # if previous 2 characters have the same case, use the opposite 
    if (i > 2 && all(grepl(is.upper, chars[i-seq_len(2)]))) {
      transform <- tolower
    }
    else if (i > 2 && all(grepl(is.lower, chars[i-seq_len(2)]))) {
      transform <- toupper
    }
    else {
      transform <- sample(list(toupper, tolower), 1)[[1]]
    }
    chars[[i]] <- transform(chars[[i]])
  }
  return(paste(chars, collapse = ""))
}

set.seed(12358)
MixedCases("This is useless R Function that seems to exists.")
