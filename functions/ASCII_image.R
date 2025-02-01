#install.packages("jpeg")
library(jpeg)
library(dplyr)

#C=strsplit('851+-. ','')[[1]]
#i=jpeg::readJPEG(system.file('img/Rlogo.jpg',package='jpeg'))[c(T,F),,2]
#i[]=C[findInterval(i,quantile(i,p=seq(0,1,,length(C))))]
#cat(apply(i,1,paste,collapse=''),sep='\n')


next_char <- list(
  ' ' = ' -+',
  '0' = '-+|17235469*80&',
  '1' = '-+|17235469*80&',
  '2' = '-+|17235469*80&',
  '3' = '-+|17235469*80&',
  '4' = '-+|17235469*80&',
  '5' = '-+|17235469*80&',
  '6' = '-+|17235469*80&',
  '7' = '-+|17235469*80&',
  '8' = '-+|17235469*80&',
  '9' = '-+|17235469*80&',
  '+' = ' -+!10',
  '-' = ' -+!10',
  '!' = ' -+!10',
  '*' = ' -+!10',
  '|' = ' -+!10',
  '&' = ' -+!10'
) %>% purrr::map(~rev(strsplit(.x, '')[[1]]))

char_lengths <- next_char %>% 
  purrr::map_int(length) %>% 
  unique() 


gamma <- 1.3



asciify <- function(jpeg_filename, gamma) {

  qimage <- list()
  
  

  image <- jpeg::readJPEG(jpeg_filename)
  

  if (length(dim(image))==3) {
    image <- image[,,2]
  }
  image <- image[c(T,F),]
  
  for (char_length in char_lengths) {
    probs <- seq(0, 1, length.out = char_length + 1)
    j     <- image
    j[]   <- findInterval(image, quantile(image, probs = probs^gamma), rightmost.closed = TRUE)
    qimage[[char_length]] <- j
  }
  

  select_next_char <- function(row, col, prev_char) {
    
    # What are the allowable next characters?
    available_chars <- next_char[[prev_char]]
    N               <- length(available_chars)
    
    # Which quantised image should be used as the reference?
    this_qimage     <- qimage[[N]]
    
    # What is the level/value at the current location
    level           <- this_qimage[row, col]
    level           <- min(level, N)
    
    available_chars[level]
  }
  
  prev_char <- '+'
  ascii <- image
  
  for (row in seq(nrow(image))) {
    for (col in seq(ncol(image))) {
      if (col == ncol(image)) {
        # If we're at the end of a row, must end in a + or -
        this_char <- '+'
      } else {
        # Otherwise choose a character based upon the previous one
        this_char <- select_next_char(row, col, prev_char)
      }
      ascii[row, col] <- this_char
      prev_char       <- this_char
    }
  }
  

  ascii[nrow(ascii), ncol(ascii)] <- 0
  
  ascii <- paste(
    apply(ascii, 1, paste, collapse=''),
    collapse="\n"
  ) 
  
  ascii
}




ascii <- asciify("/Users/tomazkastrun/Downloads/tk_2022.jpg", gamma=2.0) 
cat(ascii, "\n")

write.csv(ascii,"/Users/tomazkastrun/Downloads/TK_personal_photo_2022.txt", row.names = FALSE)
_