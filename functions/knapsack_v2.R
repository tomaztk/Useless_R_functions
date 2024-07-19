knapsack <- function(values, weights, n, W) {
  # Initialize matrix to store results
  m <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  # Fill the matrix using dynamic programming
  for (i in 1:n) {
    for (w in 1:W) {
      if (weights[i] > w) {
        m[i + 1, w] <- m[i, w]
      } else {
        m[i + 1, w] <- max(m[i, w], m[i, w - weights[i]] + values[i])
      }
    }
  }
  
  # Trace back to find the items included in the knapsack
  included_items <- integer(n)
  k <- W
  for (i in n:1) {
    if (m[i + 1, k] != m[i, k]) {
      included_items[i] <- 1
      k <- k - weights[i]
    }
  }
  
  # Return the maximum value and the items included
  return(list(max_value = m[n + 1, W], included_items = included_items))
}

# Example usage
values <- c(60, 100, 120)
weights <- c(10, 20, 30)
n <- length(values)
W <- 50

solution <- knapsack(values, weights, n, W)
max_value <- solution$max_value
included_items <- solution$included_items

print(paste("Maximum value:", max_value))
print("Items included in the knapsack:")
for (i in 1:n) {
  if (included_items[i] == 1) {
    print(paste("Item", i, "with value", values[i], "and weight", weights[i]))
  }
}



## v2

values <- c(60, 100, 120)
weights <- c(20, 20, 10)
capacity <- 50

knapsack <- function(values, weights, capacity) {
  n <- length(values)
  dp <- matrix(0, nrow = n + 1, ncol = capacity + 1)
  
  for (i in 1:n) {
    for (w in 0:capacity) {
      if (weights[i] <= w) {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w + 1 - weights[i]] + values[i])
      } else {
        dp[i + 1, w + 1] <- dp[i, w + 1]
      }
    }
  }
  
  return(dp[n + 1, capacity + 1])
}


print(knapsack(values, weights, capacity))
