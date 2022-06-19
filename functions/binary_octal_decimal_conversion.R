##########################################
# 
# Plotting decimal to binary conversion
# 
# Series:
# Little Useless-useful R functions #39
# Created: June 20, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


## Function to convert binary to decimal
bin2dec <- function(bin){
  
  bin_ss <- as.numeric(strsplit(as.character(bin),"")[[1]])
  pwr <- c((length(bin_ss)-1):0)
  res <- 0
  for (i in 1:length(bin_ss)){
    res <- res + bin_ss[i]*(2**pwr[i])
  }

return(paste("Binary", bin, "is ",res," in decimal"))
}


## Function to convert decimal to binary
dec2bin <- function(dec){
  dec_start <- dec
  str <- ''
  while (dec > 0) {
      if ((dec %% 2)==1){
        str <- paste0(str, '1')
      } else { #((dec %% 2)==0)
        str <- paste0(str, '0')
      }
    dec <- floor(dec/2)
  }
  splits <- strsplit(str, "")[[1]]
  reversed <- rev(splits)
  f_str <- paste(reversed, collapse = "")
  #return(paste("Decimal", dec_start, "is ",f_str," in binary"))
  return(f_str)
}


# Test
bin2dec(11101100)
dec2bin(236)





### Draw scatter plot for the conversion
df <- data.frame(dec_x = 1, bin_y = 1, digit_length=1)

for (i in 2:100){
    d <- c(dec_x = i, bin_Y = dec2bin(i), nchar(dec2bin(i)))
   df <-  rbind(df,d)
}

# change formats
df$dec_x <- as.numeric(df$dec_x)
df$bin_y <- as.numeric(df$bin_y)


library(ggplot2)
library(cowplot)

line <- ggplot(df, aes(x=dec_x, y=bin_y, colour=digit_length)) + geom_line()
line2 <- ggplot(df, aes(x=dec_x, y=bin_y)) + geom_line() + geom_smooth()

plot_grid(line, line2, labels = c("Length of binary digits", "Like a binominal distribution"))


