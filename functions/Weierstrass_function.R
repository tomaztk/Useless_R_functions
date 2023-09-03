
# Weierstrass function

# 0 < a < 1
# b odd positive integer
# ab > 1 + 3/2 pi



weierstrass <- function(x, a = 0.5, b = 3) {
  result <- 0
  for (n in 0:100) {  
    result <- result + (a^n * cos(b^n * pi * x)) }
 return(result)
}

len <- 1000
a <- 0.5  # 0 < a < 1
b <- 3    # b odd positive integer
x <- seq(-2, 2, length.out=len)  
y <- weierstrass(x, a, b)


plot(x, y, type = "l", col = "red", lwd = 1, main = "Weierstrass curve")


