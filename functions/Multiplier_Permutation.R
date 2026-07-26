# Useful useless function R function

# Find numbers n (0 < n <= max_n) for which there exists at
# least one integer multiplier k >= 2 such that k*n is a digit
# permutation ("anagram") of n itself.
#
# Classic example: 142857
#   142857 * 2 = 285714  (same digits, different order)
#   142857 * 3 = 428571
#   142857 * 4 = 571428
#   142857 * 5 = 714285
#   142857 * 6 = 857142

digit_counts <- function(x) {
  counts <- integer(10)
  if (x == 0) {
    counts[1] <- 1
    return(counts)
  }
  while (x > 0) {
    d <- x %% 10
    counts[d + 1] <- counts[d + 1] + 1
    x <- x %/% 10
  }
  counts
}

n_digits <- function(x) if (x == 0) 1L else as.integer(floor(log10(x)) + 1)


find_permutation_multiples <- function(max_n = 1e6) {
  
  numbers     <- integer(0)
  n_valid_ks  <- integer(0)
  ks_list     <- list()
  
  for (n in 1:max_n) {
    dn  <- n_digits(n)
    cn  <- digit_counts(n)
    hit <- integer(0)
    
    for (k in 2:9) {
      prod <- n * k
      if (n_digits(prod) != dn) break        
      if (identical(digit_counts(prod), cn)) hit <- c(hit, k)
    }
    
    if (length(hit) >= 1) {
      numbers    <- c(numbers, n)
      n_valid_ks <- c(n_valid_ks, length(hit))
      ks_list[[as.character(n)]] <- hit
    }
  }
  
  data.frame(number = numbers, n_multipliers = n_valid_ks)
}

results <- find_permutation_multiples(1e6)

head(sort(results$n_multipliers, decreasing=TRUE),10)
