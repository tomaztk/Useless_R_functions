# #################
#
# Little useless-useful R functions 
#   Solution with O(n) Time and O(n) Space with 
#    R on for CanSum() problem
#
#
# #################

t <- 7
n <- c(5,3,4,7)


## Using Brute Force

canSumBF <- function(target, numbers){
  if (target == 0) {
    return (TRUE)
  }
  
  if (target < 0){
    break;
    return (FALSE)
  }
  
  for (i in 1:length(numbers)){
    remainder <- target - numbers[i] 
    print(remainder)
    if (canSumBF(remainder, numbers) == TRUE) {
      return (TRUE)
    } 

  }
  
  break;
  return(FALSE)
  }

#test
canSumBF(t, n)







