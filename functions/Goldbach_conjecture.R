
##########################################
#
#
# Goldbach's Conjecture
# Two primes for a given sum (even int)
#
# Series:
# Little Useless-useful R functions #52
# Created: Jul 15, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################

# sieve of sundaram
sieve_of_sundaram <- function(limit) {
  n <- (limit - 1) %/% 2
  sieve <- rep(TRUE, n + 1)
  
  for (i in 1:n) {
    j <- 1
    while (i+j+2*i*j <= n) {
      sieve[i+j+2*i*j] <- FALSE
      j <- j + 1
    }
  }
  primes <- c(2, (2 * (1:n)+1)[sieve])
  return(primes)
}

#limit <- 100
#prime_numbers <- sieve_of_sundaram(limit)



nth_max <- 100
kl <- list(
   k <- 1:as.integer(floor(2.4 * nth_max * log(nth_max) / 2))
  ,integers_list <- replicate(length(k),FALSE)
)

pcount <- 0
for (i in (1:k+1)){
  if (is.na(integers_list[i]) == TRUE){
    pcount <- pcount + 1
    if (pcount %% 10 == 0)  {
      print(k)
    }
  }
}



