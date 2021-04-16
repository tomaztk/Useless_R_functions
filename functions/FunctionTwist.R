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
    #return ((n * n+1) + sum_ser(n - 1))
    return (0)
  } else {
    return ((n * n) + sum_ser(n - 1))
  }
}

sum_ser(0)



#### Info about stack
Cstack_info()


##### similar circular function

change_to_factor <- function(x){
  x <- change_to_character(x)
  as.factor(x)
} 

change_to_character <- function(x){
  x <- change_to_factor(x)
  as.character(x)
}

change_to_character("1")


### Another recursive

rn <- function(a=2){
  res <- sample(a)
  rn(res)
}

rn()

