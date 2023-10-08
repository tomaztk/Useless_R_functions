
##########################################
# 
# Reading faster with Bionic Reading
#
# Series:
# Little Useless-useful R functions #56
# Created: October 07, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################



sample_text <- "
Bionic Reading is a new method 
facilitating the reading process
by guiding the eyes through 
text with artificial fixation points.

As a result, the reader is only 
focusing on the highlighted 
initial letters and lets the brain 
center complete the word.

In a digital world dominated 
by shallow forms of reading,
Bionic Reading aims to 
encourage a more in-depth 
reading and understanding 
of written content.
"


Make_text_easier_to_read <- function(input_text){
  
  bold <- "\033[1m"
  underline <- "\033[4m"
  reset <- "\033[0m"
  blue <- "\033[34m"

  modify_word <- function(word) {
    word_length <- nchar(word)
    first_half <- substr(word, 1, ceiling(word_length / 2))
    first_half_bold <- paste0(bold, first_half, reset)
    second_half <- substr(word, ceiling(word_length / 2)+1, word_length)
    second_half_bold <- paste0(blue, second_half, reset)
    final_word <- paste0(first_half_bold, second_half_bold)
    return(final_word)
  }
  
    words <- unlist(strsplit(sample_text, " "))
    modified_words <- sapply(words, modify_word)
    formatted_text <- paste(modified_words, collapse = " ")
    cat(formatted_text, "\n")
}




#simple text
cat("\033[34m", sample_text, "\033[0m", "\n")

# run the function
Make_text_easier_to_read(sample_text)





