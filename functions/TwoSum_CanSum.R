##########################################
#
#
# Solution with O(n) Time and O(n) Space with 
# R on for CanSum() problem
#
# Series:
# Little Useless-useful R functions #43
# Created: October 15, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################



## Using Brute Force
#' Each step of the subtree will be calculated again

canSumBF <- function(target, numbers){
  if (target == 0) { return (TRUE) }
  if (target < 0){ return (FALSE) }
  
  for (i in 1:length(numbers)){
    remainder <- target - numbers[i] 
    if (canSumBF(remainder, numbers) == TRUE) {
      return (TRUE)
    }
  }
  return(FALSE)
}


#combo test
canSumBF(7, c(2,3,5)) ## true
canSumBF(8, c(5,3,4,7)) ## true
canSumBF(87, c(13,10)) ## false
canSumBF(250, c(7,14)) ## false ... takes cca 45 sec :)


## Using memos for intermediate states 
#' Calling recursive function and store
#' intermediate states of subtree calculations
#' to diminish the number of recursions
canSumMEMO <- function(target, numbers, memo = list()){
  if (target == 0) { return (TRUE) }
  if (target < 0)  { return (FALSE) }
  if (target %in% names(memo)) {
    return (memo[[as.character(target)]])
  }
  
  for (i in 1:length(numbers)){
    remainder <- target - numbers[i]
    # Fixed version to emulate behaviour of canSumBF
    if (canSumMEMO(remainder, numbers, memo) == TRUE) {
     #if (canSumMEMO(remainder, numbers[i], memo) == TRUE) {
      memo[[as.character(target)]] <- TRUE
      return (TRUE)
    }
  }
  memo[[as.character(target)]] <- FALSE;
  return(FALSE)
  
}

# test memo
canSumMEMO(250, c(7,14)) ## false ...superfast :)
canSumMEMO(150, c(7,14)) ## false 
canSumMEMO(8, c(5,3,4,7)) ## true





########## compare both solutions

startBF <- Sys.time()
canSumBF(250, c(7,14)) 
endBF <- Sys.time()
timeBF <- endBF - startBF

startMEMO <- Sys.time()
canSumMEMO(250, c(7,14)) 
endMEMO <- Sys.time()
timeMEMO <- endMEMO - startMEMO

