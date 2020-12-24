##########################################
# 
# Reading scrambled text
#
# Series:
# Little Useless-useful R functions #9
# Created: December 24, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - 
###########################################


#helper function
full_scramble <- function(s_word) {
  s_word <- as.character(s_word)
  i <- sample(1:nchar(s_word))
  sep_word <- unlist(strsplit(s_word, ""))
  paste(sep_word[i], collapse = "")
}


WordScrambler <- function(text){

  Words <- as.list(el(strsplit(Text, " ")))
  New_text <- paste0(unlist(sapply(1:length(Words), function(x) full_scramble(Words[x]))), collapse = " ")
  
  return(tolower(New_text))
}


#Get some sample text
Text <- "This is a successful writting of the quick brown fox jumps over the lazy dog"

#run the Word Scrambler
WordScrambler(Text)
