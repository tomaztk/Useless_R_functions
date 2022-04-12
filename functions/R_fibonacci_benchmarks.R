library(numbers)

# number of folds
n <- 10

fib1 <- function(n){
  res <- 0
  if (n == 1 | n == 2) {res <- 1}
  if (n >= 3) {res <- fib1(n-1) + fib1(n-2) }
  #return (res)
  res_fib1 <<- res
}

 

fib2 <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    #return(fib2(n-1) + fib2(n-2))
    res_fib2 <<- fib2(n-1) + fib2(n-2)
  }
}

 

fib3 <- function(n){
  
  fibvals <- numeric(n)
  fibvals[1] <- 1
  fibvals[2] <- 1
  for (i in 3:n) { 
    fibvals[i] <- fibvals[i-1]+fibvals[i-2]
  }
  #return (tail(fibvals, n=1))
  res_fib3 <<- tail(fibvals, n=1)
}


fib4 <- function(n) {
        res_fib4 <<- tail( round(((5 + sqrt(5)) / 10) * (( 1 + sqrt(5)) / 2) ** (1:n - 1)), n=1)
}

fib5 <- function(n){
    res_fib5 <<- fibonacci(n, sequence = FALSE)
  }


# Test correctness

start_time1 <- Sys.time()
  fib1(n)
end_time1 <- Sys.time()
res1_time <- end_time1 - start_time1

start_time2 <- Sys.time()
fib2(n)
end_time2 <- Sys.time()
res2_time <- end_time2 - start_time2

start_time3 <- Sys.time()
fib3(n)
end_time3 <- Sys.time()
res3_time <- end_time3 - start_time3

start_time4 <- Sys.time()
fib4(n)
end_time4 <- Sys.time()
res4_time <- end_time4 - start_time4

start_time5 <- Sys.time()
fib5(n)
end_time5 <- Sys.time()
res5_time <- end_time5 - start_time5



res1_time
res2_time
res3_time
res4_time
res5_time
