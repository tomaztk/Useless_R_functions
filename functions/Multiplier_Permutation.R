# Useful useless function
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

plot(
  results$number, results$n_multipliers,
  pch = 19, cex = 0.7, col = adjustcolor("#2C7FB8", alpha.f = 0.6),
  xlab = "Number (0 - 1,000,000)",
  ylab = "Count of valid multipliers (k = 2..9)",
  main = "Numbers whose digits get shuffled by multiplication",
  las = 1
)
best <- results[which.max(results$n_multipliers), ]
points(best$number, best$n_multipliers, pch = 21, cex = 1.6, col = "red", lwd = 2)
text(best$number, best$n_multipliers, labels = best$number, pos = 3, col = "red", cex = 0.8)

