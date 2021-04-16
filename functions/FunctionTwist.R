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


######## circle functions without exit

fn1 <- function(n){
  return(n*n*fn2(n))
}


fn2 <- function(m){
  return((m+m))  #+fn2(m-1)) ## We need to give it a limit; without refencing self or reducing steps m-1
  
}

fn1(3)
fn2(3)


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
