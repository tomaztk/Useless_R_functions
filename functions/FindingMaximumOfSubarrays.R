##########################################
# 
# Total Sum of sub-array
# 
#
# Series:
# Little Useless-useful R functions #37
# Created: April 29, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


arr = c(1,2,3,4)

sumArr <- function(x){
  summ <- 0
  i <- 1
  for (i in 1:length(x)) {
    j <- i + 0
    midsum <- 0
    for (j in j:length(x)) {
      midsum <- sum(x[i:j])
      summ <- summ + midsum
      #print(sum)
    }
  }
  cat(paste0("Total sum of sub-arrays: ", summ))
 }

sumArr(arr)



sumArrOfMax <- function(x){
  summ <- 0
  i <- 1
  for (i in 1:length(x)) {
    j <- i + 0
    midsum <- 0
    for (j in j:length(x)) {
      midsum <- max(x[i:j])
      summ <- summ + midsum
      #print(sum)
    }
  }
  cat(paste0("Total sum of maximums of all sub-arrays: ", summ))
}


sumArrOfMax(arr)


set.seed(2908)
#making bigger array
arr2 <-  as.numeric(sample(-100:100, 1000, replace=T))

# script stopped after 1 min
sumArrOfMax(arr2)
# 48690847 #running: 3 sec



# Divide and conquer solutions?

