##########################################
# 
# Generate wacky (strong) password
# Series:
# Little Useless-useful R functions #8
# Created: November 5, 2020 - work in prog
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


WackyPassword <- function(WP_length, WP_level=TRUE){

  WP_length <- 20
  charblock1 = c(176:178, 185: 188, 200:206)
  charblock2 = c(73,105,108,124,49,33)
  numberblock3 <- sample(0:9, length(5),replace = TRUE)
  

   if (WP_level==TRUE){
 
     
     stopifnot(is.numeric(WP_length),  WP_length   > 0L,
               is.character(charblock1), charblock1  > 0L, charblock1 <= WP_length,
               is.numeric(charblock2), charblock2  > 0L, charblock2 <= WP_length,
               is.numeric(numberblock3), numberblock3  > 0L, numberblock3 <= WP_length,
               length(intersect(charblock1, charblock2, numberblock3)) == 0L)
        
     wacky <- charblock1
     pass[capitals] <- uc
     pass[numbers]  <- num
     
     paste0(pass, collapse = "")
     
  }
  if (WP_level==FALSE){
    
    rerw
  }
  
  data.frame(coderange,as.raw(coderange),row.names=rawToChar(as.raw(coderange),multiple=TRUE))
  
}


gen_pass <- function(len=8,seeder=NULL){
  set.seed(seeder) # to allow replicability of passw generation
  # get all combinations of 4 nums summing to length len
  all_combs <- expand.grid(1:(len-3),1:(len-3),1:(len-3),1:(len-3))
  sum_combs <- all_combs[apply(all_combs, 1, function(x) sum(x)==len),]
  # special character vector
  punc <- unlist(strsplit("!#$%&’()*+,-./:;<=>?@[]^_`{|}~",""))
  # list of all characters to sample from
  chars <- list(punc,LETTERS,0:9,letters)
  # retrieve the number of characters from each list element 
  # specified in the sampled row of sum_combs
  pass_chars_l<- mapply(sample, chars,
                        sum_combs[sample(1:nrow(sum_combs),1),],
                        replace = TRUE)
  # unlist sets of password characters 
  pass_chars <- unlist(pass_chars_l)
  # jumble characters and combine into password 
  passw <- str(sample(pass_chars),collapse = "")
  return(passw)
}


gen_pass(8,29081978)




