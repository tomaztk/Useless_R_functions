
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


nth_max <- 100
kl <- list(
   k <- 1:as.integer(floor(2.4 * nth_max * log(nth_max) / 2))
  ,integers_list <- replicate(length(k),FALSE)
)
#k <- as.integer(floor(2.4 * nth_max * log(nth_max) / 2))
#integers_list <- TRUE * k

for (i in 1:k){
  j <- 1
  while(i+j+2*i*j < k){
    j <- j + 1
    integers_list[i+j+2*i*j] <- FALSE
  }
}
pcount <- 0
for (i in (1:k+1)){
  if (is.na(integers_list[i]) == TRUE){
    pcount <- pcount + 1
    if (pcount %% 10 == 0)  {
      print(k)
    }
  }
}
