# #################
#
# Little useless-useful R functions 
#   Solution with O(n) Time and O(n) Space with 
#   R on for CanSum() problem
#
#
# #################


## Using Brute Force
canSumBF <- function(target, numbers){
  if (target == 0) { return (TRUE) }
  if (target < 0){ return (FALSE) }
  
  for (i in 1:length(numbers)){
    remainder <- target - numbers[i] 
    if (canSumBF(remainder, numbers) == TRUE) {
      return (TRUE)
    }
  }
  return(FALSE)
}

t <- 7
n <- c(5,3,4,7)



#test Brute Force
canSumBF(t, n) 

#combo test
canSumBF(7, c(5,3,4,7)) ## true
canSumBF(250, c(7,14)) ## false ... takes cca 45 sec :)


## Using memos for intermediate states 


