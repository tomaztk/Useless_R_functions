
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
  primes <- c(2,(2*(1:n)+1)[sieve])
  return(primes)
}


#list of all primes until "limit"
#sieve_of_sundaram(limit)



# is prime
is_prime <- function(n) {
  if (n <= 1)  return(FALSE)
  if (n <= 3)  return(TRUE)
  if (n %% 2 == 0 || n %% 3 == 0) return(FALSE)
  i <- 5
  while (i*i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) return(FALSE)
    i <- i + 6
  }
  return(TRUE)
}
  

## goldbach for even numbers
goldbach_conjecture <- function(even_num) {
  if (even_num <= 2 || even_num %% 2 != 0) {
    return("Number must be even and greater than 2.")
  }
  c <- NULL
  for (i in 2:(even_num / 2)) {
    if (is_prime(i) && is_prime(even_num - i)) {
      #cat("Goldbach's pairs for", even_num, "are:", i, "+", even_num - i, "\n")
      c <- cbind(c,i) # nof solutions
    } 
  }
  #return(length(c))
  return(c)
}

# test
goldbach_conjecture(870)


#make some 1000 solutions
sol <- NULL
for (i in seq(4,1000, by=2)){
  nof_solutions <- goldbach_conjecture(i)
  sol <- rbind(sol, data.frame(n=i, nof=nof_solutions))
}


# plot solutions; alternating solutions
plot(sol$n, sol$nof, type = "p", xlab = "Even number", ylab = "Number of Solutions", main = "Goldbach's Conjecture") 
reg<-lm(nof ~ n, data = sol)
abline(reg, col="red")


# most frequent primes:
fre <- NULL
for (i in seq(4,1000, by=2)){
  sols <- goldbach_conjecture(i)
  fre <- cbind(fre, sols)
}

# prepare solutions
solutions_freq<- data.frame(table(t(fre)))

# visualisation
solutions_freq <- solutions_freq[which(as.integer(solutions_freq$Freq) > 1),]
plot(x=solutions_freq$Var1, y=solutions_freq$Freq,
     xlab = "Prime number", ylab = "Frequency of prime in sum", main = "Frequencies of prime numbers for
     Goldbach's Conjecture for first \n 1000 even integers.")

