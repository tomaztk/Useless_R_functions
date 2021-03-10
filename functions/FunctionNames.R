# combine

a <- c(0,0,0,0.2)

c <- function(a,b){
  return("aaaa")
}

a <- c("a","b")
#error
a <- c(0,0,0,0.2)

# True / False

False <- FALSE
True <- TRUE

isTRUE(True)
identical(True, TRUE)


# sum function
sum <- function(x){
  mean(x)
}
sum(c(10,20,40,20))



#Letters

LETTERS
letters

#numbers

numbers <- c(0,1,2,3,4,5,6,7,8,9)

NUMBERS <- c("0","1","2","3","4","5","6","7","8","9")

numbers
NUMBERS



ca <- function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
  )
}


mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

mean_ci(10:15)
sd(c(10,22,23,45))


