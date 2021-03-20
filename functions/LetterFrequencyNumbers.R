##########################################
# 
# Letter frequency for numbers in a dataset
# Series:
# Little Useless-useful R functions #22
# Created: MARCH 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
 # words of numbers:
 # https://www.woodwardenglish.com/lesson/numbers-1-100-in-english/
###########################################


#function
word_a_number <- function(numb){

  basLet <- c('one','two','three','four','five','six','seven','eight','nine','ten'
              ,'eleven','twelve','thirteen','fourteen','fifteen','sixteen','seventeen','eighteen','nineteen'
              ,'twenty','thirty','forty','fifty','sixty','seventy','eighty','ninety','one hundred')
  basNum <- c(1:20,30,40,50,60,70,80,90,100)
  df <- data.frame(num = basNum, let = as.character(basLet))
  
          if (numb <= 20) {
            im <- df[which(df$num == numb),]$let
            #print(paste(im, collapse = NULL))
          } else {
                  if (numb %% 10 == 0){
                    e <- df[which(df$num == numb),]$let
                    #print(paste0(e, collapse=NULL))
                  } else {
                    sec <- numb %% 10
                    fir <- as.integer(numb/10)*10
                    f_im <- df[which(df$num == fir),]$let
                    s_im <- df[which(df$num == sec),]$let
                    res <- paste0(f_im,"-",s_im, collapse = NULL)
                    #print(res)
                  }
          }
}

# run a single function
word_a_number(87)


#function for  count the frequency 
freqLet <- function(x) {
  word <- tolower(unlist(strsplit(x,"")))
  word_table <- table(word)
  ans <- word_table[names(word_table)]
}

getFreq <- function(vect) {
  df <- data.frame(word=as.character(), stringsAsFactors = FALSE)
  for (i in 1:length(vect)) {
    df[i,1] <- as.character(word_a_number(i))
    a <<- freqLet(df$word)
  }
  return(a)
}

######### Let's check the complete set of numbers

# Automate the function, get a vector of first 100 numbers
vect <- c(1:100)

#Is there A in first 100 words?
getFreq(vect)

#quick visual
plot(sort(a, decreasing = TRUE), type = "h", col = "red", lwd = 10,
     main = "Letter frequency in numbers", xlab = "Letters", ylab = "frequnecy of a letter")

