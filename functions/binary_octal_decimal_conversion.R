##################
#
# Conversion to-from
# Binary, octal, Decimal
#
##################


bin2dec <- function(bin){
  
  bin_ss <- as.numeric(strsplit(as.character(bin),"")[[1]])
  pwr <- c((length(bin_ss)-1):0)
  res <- 0
  for (i in 1:length(bin_ss)){
    res <- res + bin_ss[i]*(2**pwr[i])
  }

return(paste("Binary", bin, "is ",res," in decimal"))
}


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
  return(paste("Decimal", dec_start, "is ",f_str," in binary"))
}


# Test
dec <- 236
bin <- 11101100

bin2dec(bin)
dec2bin(dec)

