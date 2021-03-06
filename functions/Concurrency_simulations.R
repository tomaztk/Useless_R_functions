##########################################
# 
# Running concurrent simulations
#
# Series:
# Little Useless-useful R functions #24
# Created: June 4, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


computelinks <- function(links){
  nr <- nrow(links)
  nc <- ncol(links)
  tot = 0
  for (i in 1:(nr-1)) {
    for (j in (i+1):nr) {
      for (k in 1:nc)
        tot <- tot + links[i,k] * links[j,k]
    }
  }
  r <- tot/(nr*(nr-1)/2)
 print(r)
}


sim_Slow <- function(nr,nc){
  cal <-  matrix(sample(0:1, (nr*nc), replace=TRUE), nrow=nr)
  system.time(computelinks(cal))
}


## Optimised version
computelinks_fast <- function(links){
  nr <- nrow(links)
  nc <- ncol(links)
  tot <- 0
  for (i in 1:(nr-1)) {
    tmp <- links[(i+1):nr,] %*% links[i,]
    tot <- tot + sum(tmp)
  }
  
  r <- tot/(nr *(nr-1)/2)
  print(r)
}


sim_Fast <- function(nr,nc){
  cal <- matrix(sample (0:1, (nr*nc), replace=TRUE), nrow=nr)
  print(system.time(computelinks_fast(cal)))
}

###################################
### Comparison of both calculations
###################################

sim_Slow(1000,1000)
sim_Fast(1000,1000)



###################################
### Library parallel
### WIP
###################################


library(parallel)

doichunk <- function (ichunk) {
  tot <- 0
  nr <- nrow(lnks) 
  for (i in ichunk) {
    tmp <- lnks[(i+1):nr,] %∗% lnks[i,]
    tot <- tot + sum(tmp)
  }
  tot
}


mutoutpar <- function(cls,lnks) {
  nr <- nrow(lnks)
  clusterExport(cls, "lnks")
  ichunks <- 1:(nr-1)
  tots <- clusterApply(cls, ichunks, doichunk)
  Reduce(sum,tots)/nr
}

makeCluster(nworkers)
cls <- initmc(4)
length(cls)
cl2 <- initcls(c("pc28" ,"pc29"))
clusterExport(cls,"lnks")

lnks <<- matrix(sample(0:1,(nr*nc),replace=TRUE),nrow=nr)
clusterExport(cls, "lnks" , envir=environment())

ichunks <- 1:(nr-1)
tots <- clusterApply(cls, ichunks, doichunk)
sum(tots)
