mutoutser <- function(links){
  nr <- nrow(links)
  nc <- ncol(links)
  tot = 0
  for (i in 1 : ( nr - 1)) {
    for (j in ( i + 1): nr) {
      for (k in 1 : nc)
        tot <- tot + links[i,k] * links[j,k]
    }
  }
  tot / (nr * (nr - 1) / 2 )
}


sim <- function(nr,nc) {
  lnk <-  matrix( sample ( 0 : 1 , ( nr*nc ) , replace=TRUE) ,
                  nrow=nr )
  system.time ( mutoutser ( lnk ) )
}

sim(1000,1000)


########



mutoutser1 <- function (links) {
  nr <- nrow(links)
  nc <- ncol(links)
  tot <- 0
  for ( i in 1 : ( nr - 1)) {
    tmp <- links[ ( i + 1): nr,] %*% links[i,]
    tot <- tot + sum(tmp)
  }
  tot / nr
}


sim1 <- function(nr,nc) {
  lnk <- matrix(sample (0:1 ,(nr*nc), replace=TRUE) ,
                  nrow=nr)
  print (system.time(mutoutser1(lnk)))
}

sim(1000,1000)
