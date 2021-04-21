

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



# social distancing sort :-)
# Like bubble but with distance

SocialDistancing_sort = function(ser) {
  stevec <- 0
  
  while(TRUE) {
    stev_menjava <- 0
    for (j in 1 : (NROW(ser) - stevec - 1)) {
      if (ser[j] > ser[j + 1]) {
        s <- ser[j]
        ser[j] <- ser[j+1]
        ser[j+1] <- s
        stev_menjava =+  1
      }

    }
    stevec =+ 1

        if(stev_menjava == 0) break
        }
  cat(ser)
}

SocialDistancing_sort(series)

