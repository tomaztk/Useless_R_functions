###################################
#
# function to call a function
# Function twist
# Circular or recursive functions
#
###################################

fun1 <- function(x, num){
  
    #res <- x(num)
    res <- do.call(x, num)
    return(res)
}


fun2 <- function(num){
  
  return(num*num)
}

fun2(4)
fun1(fun2, 4)
fun1(fun2,list(4))



######## circle functions without exit

fn1 <- function(n){
  return(n*n*fn2(n))
}


fn2 <- function(m){
  return((m+m))  #+fn2(m-1)) ## We need to give it a limit; without refencing self or reducing steps m-1
  
}

fn1(3)
fn2(3)


#### example with recursion

sum_ser <- function(n)
{
  if(n == 0) {
    #return ((n * n+1) + sum_ser(n - 1))
    return (0)
  } else {
    return ((n * n) + sum_ser(n - 1))
  }
}

sum_ser(4)



#### Info about stack
Cstack_info()


##### similar circular function
## Changing from list to vector hitting stack limit (classical case)

to_vector <- function(x){
  x <- to_list(x)
  as.list(x)
} 

to_list <- function(x){
  x <- to_vector(x)
  as.vector(x)
}

to_vector(2908)



### Another function; calling itself 

rn <- function(a=2){
  res <- sample(a)
  rn(res)
}

rn(10)



### Recursions in Mathematics (Factorial / Fibonnaci )

fact <- function(x){
  if(x == 0){
    return(0)
  } 
  if(x==1){
    return(1)
  } else {
    return(x*fact(x-1))
  }
}

fact(4)

fact(-4)

## Fibonacci sequence
recurse_fibonacci_sum <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(recurse_fibonacci_sum(n-1) + recurse_fibonacci_sum(n-2))
  }
}  

print_fibonacci <- function(x){
  vc <- c()
  for(i in 0:(x-1)) {
    vc <- c(vc, recurse_fibonacci_sum(i))
    }
    cat(vc)
}



print_fibonacci(15)

#################################
### Simple sorting with recursion
#################################

quacksort <- function(setN){
  if(length(setN)<=1 | length(setN)==0) {
    return(setN)
  } else {
    home <- setN[1]
    rest <- setN[-1]
    rest_set <- rest[rest > home]
    home_set <- rest[rest <= home]
    rest_set <- quacksort(rest_set)
    home_set <- quacksort(home_set)
    return((c(home_set,home,rest_set)))
  }
}

series <- c(65,963,12,-256,529,57,12,778, 0, 54,333,-12345,12, 1,43423,5,7786,43,23,5,67,9098,5,33,22)

#series
quacksort(series)

#single number
quacksort(4)

