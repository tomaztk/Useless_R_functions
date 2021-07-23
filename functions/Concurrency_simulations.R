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
  tot <- 0
  for (i in 1:(nr-1)) {
    for (j in (i+1):nr) {
      for (k in 1:nc)
        tot <- tot + links[i,k] * links[j,k]
    }
  }
  r <- tot/(nr*(nr-1)/2)
 print(r)
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


sim_Fast <- function(mat){
  print(system.time(computelinks_fast(mat)))
}

sim_Slow <- function(mat){
  system.time(computelinks(mat))
}

###################################
### Comparison of both calculations
###################################

# will produce same end results; different timings
nr <- 500
nc <- 500
cal <-  matrix(sample(0:1, (nr*nc), replace=TRUE), nrow=nr)


sim_Slow(cal)
sim_Fast(cal)


###################################
### Library parallel
### WIP with same calculations
###################################

# install.packages("parallel")
library(parallel)

doichunk <- function (ichunk) {
  tot <- 0
  nr <- nrow(lnks) 
  for (i in ichunk) {
    tmp <- lnks[(i+1):nr,] %∗% lnks[i,]
    tot <- tot + sum(tmp)
  }
  return(tot)
}

mutoutpar <- function(cls,lnks) {
  nr <- nrow(lnks)
  clusterExport(cls, "lnks")
  ichunks <- 1:(nr-1)
  tots <- clusterApply(cls, ichunks, doichunk)
  Reduce(sum,tots)/nr
}

nworkers <- #integer
makeCluster(nworkers)
