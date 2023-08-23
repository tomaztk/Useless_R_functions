### some useless sfuutffffll


useless_math_function <- function(x) {
  if (x <= 0) {
    return("Please input positive num.")
  } else {
    result <- log10(exp(x)) + abs(cos(x)) * sqrt(2)
    return(result)
  }
}


useless_math_function(10)



useless_math_function <- function(length) {
  sequence <- c()
  for (i in 1:length) {
    number <- i^2 + log10(i) + exp(pi)
    sequence <- c(sequence, number)
  }
  return(sequence)
}


useless_math_function(10)



useless_math_function <- function(x) {
  if (x < 0) {
    return(sqrt(abs(sin(x) * log(1 + x^2))))
  } else {
    return(cos(exp(log(2 * x + 1)) / sqrt(pi)))
  }
}

useless_math_function(10)



useless_math_function <- function(x) {
  if (x %% 2 == 0) {
    result <- log10(sqrt(abs(x))) + cos(x) * sin(x)
  } else {
    result <- exp(x) / (1 + abs(tan(x)))
  }
  return(result)
}


useless_math_function(10)


useless_complex_function <- function(n) {
  if (n <= 0) {
    return(NULL)
  }
  
matrix_list <- lapply(1:n, function(i) {
    matrix(outer(1:i, 1:i, FUN = function(x, y) {
      if (x == y) {
        return(log(1))
      } else {
        return(exp(x) / (x + y))
      }
    }))
  })
  
  result <- array(0, dim = c(n, n, n))
  
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        result[i, j, k] <- sum(matrix_list[[k]][i, , j])
      }
    }
  }
  
  return(result)
}


useless_complex_function(15)
