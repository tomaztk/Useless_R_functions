

###################################
#
# Useless R functions
# Sort of sort functions
#
###################################

series <- c(65,963,12,-256,529,57,12,778, 0, 54,333,-12345,12, 1,43423,5,7786,43,23,5,67,9098,5,33,22)

### Simple sort function
sort(series, decreasing = FALSE)



### Simple sorting with recursion
## Quick sort moves sorted data to left, right

quacksort <- function(setN){
  if(length(setN)<=1 | length(setN)==0) {
    return(setN)
  } else {
    home <- setN[1]
    rest <- setN[-1]
    rest_set <- rest[rest > home]
    home_set <- rest[rest <= home]
    rest_set <- quacksort(rest_set)
    home_set <- quacksort(home_set)
    return((c(home_set,home,rest_set)))
  }
}

#series
quacksort(series)

### Insert sort
Intosort <- function(A){
     for (j in 2:length(A)) {
         key = A[j] 
         i = j - 1
         while (i > 0 && A[i] > key) {
             A[(i + 1)] = A[i] 
             i = i - 1
           }
         key <- A[(i + 1)]
       }
     A
   }

Intosort(series)



