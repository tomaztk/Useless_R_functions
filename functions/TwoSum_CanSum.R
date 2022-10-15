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
canSumBF(87, c(13,10)) ## false

## Using memos for intermediate states 
#' we will use intermediate states to store calculation
#' so that diminish the number of recursions


canSumMEMO <- function(target, numbers, memo = c()){
  if (target == 0) { return (TRUE) }
  if (target < 0){ return (FALSE) }
  if (target %in% memo) { return (memo[target]) }
  
  for (i in 1:length(numbers)){
    remainder <- target - numbers[i] 
    if (canSumMEMO(remainder, numbers, memo) == TRUE) {
      memo[target] <- TRUE;
      return (TRUE)
    }
  }
  memo[target] <- FALSE;
  return(FALSE)
}


# test memo
canSumMEMO(250, c(7,14)) ## false 

canSumMEMO(150, c(7,14)) ## false 


