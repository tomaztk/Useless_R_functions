# function to call a function
# Function twist

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


######## circle functions

fn1 <- function(n){
  return(n*n*fn2(n))
}


fn2 <- function(m){
  #return(fn1(m)*fn1(m)) ## We need to give it a limit
  return(fn1(m)+fn1(m-1)) ## We need to give it a limit
  
}

fn1(1)
fn2(1)


#### with recursion


sum_ser <- function(n)
{
  if(n == 0) {
    return (0)
  } else {
    return ((n * n) + sum_ser(n - 1))
  }
}

sum_ser(10)
