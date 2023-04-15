
#svd function
svd_f <- function(x,y,a=2){
  
  r <- x*y
  b <- as.integer(!is.infinite(x/y))
  
  tmp <- data.frame(
    a1 = a*b,
    a2 = b*r,
    a3 = a*r
  )
  some_fx <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  X <- some_fx(y+5)[, 1:5]
  s <- svd(X)
  D <- diag(s$d)
  X2 = s$u %*% D %*% t(s$v) 
  D2 = t(s$u) %*% X %*% s$v 
  #kronecker(outer(22,2,"+"))
  
  mm <- as.matrix(1:10, ncol=2)
  #kronecker(outer(2,20,"+"))
  kronecker(diag(1,5), mm)
  
  some_cal <- tmp$a1 * tmp$a2
  bb <- list(
    "temp_Data"= tmp,
    "weights" = some_cal,
    "svd_s" = s,
    "svd_d" = D,
    "a" = a,
    "svd_X2" = X2,
    "svd_D2" = D2,
    "graph" = plot(rs$svd_X2)

  )
  return(bb)
}


rs <- svd_f(1,2, a=4)
svd_f(1,2, a=4)



