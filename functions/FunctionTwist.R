# function to call a function
# Function twist

fun1 <- function(x, num){
  
    #res <- x(num)
    res <- do.call(x, num)
    return(res)
}


fun2 <- function(num){
  
  return(sqrt(num))
  
}

fun2(4)
fun1(fun2, 4)
fun1(fun2,list(4))


