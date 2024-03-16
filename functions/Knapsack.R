## Knapsack

knapsack <- function(values, weights, n, W) {
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  

  for (j in 1:W) {
    m[1, j] <- 0
  }
  
  for (i in 1:n) {
    m[i, 1] <- 0
  }
  

  for (i in 2:(n + 1)) {
    for (j in 1:(W + 1)) {
      if (weights[i - 1] > j) {
        m[i, j] <- m[i - 1, j]
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - weights[i - 1]] + values[i - 1])
      }
    }
  }
  

  return(m[n + 1, W + 1])
}


values <- c(3, 4, 5, 6)
weights <- c(2, 3, 4, 5)
n <- length(values)
W <- 5
result <- knapsack(values, weights, n, W)
print(result)
