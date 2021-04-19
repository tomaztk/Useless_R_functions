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




