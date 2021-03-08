##########################################
####
#### L-System
####
##########################################

A <-   matrix(c( 0   ,  0   ,  0   , 0.20), nrow=2); 
a = c(0, 0.00); 
pa = 0.01

B <- matrix(c( 0.65, -0.10,  0, 0.80), nrow=2); 
b = c(0, 1.60); 
pb = 0.65

C <- matrix(c( 0.20,  0.10, -0.26, 0.22), nrow=2); 
c = c(0, 1.60); 
pc = 0.05

D <- matrix(c(-0.25,  0.25,  0.25, 0.25), nrow=2); 
d = c(0, 0.44); 
pd = 0.05

K <- list(A, B, C, D)
k <- list(a, b, c, d)
N <- 5000

s <- sample(1:4, N, prob = c(pa, pb, pc, pd), replace = TRUE)
P <- matrix(0, nrow=2, ncol=N+1)

for (i in seq(N)) {

  M = K[[s[i]]]
  m = k[[s[i]]]
  P[,i+2] = M %*% P[,i+1] + m+0.1
}


# Plot Matrix
plot(P[1,], P[2,],as=2,pch='.',an=F,ax=F)
