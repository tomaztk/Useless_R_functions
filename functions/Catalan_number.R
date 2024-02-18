#####################
## Catalan number ###
#####################


# Function for factorial
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Function for n-th Catalan number
catalan <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(factorial(2 * n) / (factorial(n + 1) * factorial(n)))
  }
}



for (i in 0:10) {
  cat(sprintf("fun(%d) = %d\n", i, factorial(i)))
}


# Draw graph