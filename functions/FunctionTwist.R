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




fn1 <- function(n){
  return(n*n)
  
}


fn2 <- function(m){
  return(fn1(m)*fn1(m))
}


fn1(4)

fn2(4)
