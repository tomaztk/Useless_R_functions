########################
#
#
# Levenshtein distance
#
#
#######################

a <- "tomaz"
b <- "tone"
len_a <- nchar(a)
len_b <- nchar(b)




Levenshtein_distance <- function(a,b){
  len_a <- nchar(a)
  len_b <- nchar(b)
  
  if(len_a==0) return(len_b)
  if(len_b==0) return(len_a)
  if(len_a!=0 & len_b!=0){
      if(substr(a,len_a,len_a)==substr(b,len_b,len_b)){
      return (Recall(substr(a,1,len_a-1),substr(b,1,len_b-1)) )
    } else {
      return(1+min(
      Recall(substr(a,1,len_a-1),b),Recall(a,substr(b,1,len_b-1)),Recall(substr(a,1,len_a-1),substr(b,1,len_b-1))
                ) 
         )
    }
  }
}

#check distance
Levenshtein_distance(a,b)


