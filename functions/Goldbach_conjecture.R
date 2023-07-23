
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


max <- 10000
primes <- NULL
marked <-  seq(1:((max / 2) + 100))
i <- 1
j <- 1

for (i in 1:(((length(marked) -1) /2) + 1)){
  for (j in 1:(i* (i+1))) {
    a <- c((max / 2) + 1, 2 * i + 1)
    print(a)
  }
  marked[j] <- NA
}
