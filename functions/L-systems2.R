A <-   matrix(c( 0   ,  0   ,  0   , 0.16), nrow=2); 
a = c(0, 0.00); 
pa = 0.01

B <- matrix(c( 0.65, -0.10,  0.04, 0.80), nrow=2); 
b = c(0, 1.60); 
pb = 0.85

C <- matrix(c( 0.20,  0.10, -0.26, 0.22), nrow=2); 
c = c(0, 1.60); 
pc = 0.07

D <- matrix(c(-0.25,  0.26,  0.28, 0.24), nrow=2); 
d = c(0, 0.44); 
pd = 0.07

K <- list(A, B, C, D)
k <- list(a, b, c, d)


s <- sample(1:4, 1000, prob = c(pa, pb, pc, pd), replace = TRUE)
P <- matrix(0, nrow=2, ncol=N+1)

for (i in seq(N)) {

  M = K[[s[i]]]
  m = k[[s[i]]]
    P[,i+2] = M %*% P[,i+1] + m
}


# Plot Matrix
plot(P[1,], P[2,],as=2,pch='.',an=F,ax=F)
