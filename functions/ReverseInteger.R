##########################################
#
#
# Reverse Integer
#
#
# Series:
# Little Useless-useful R functions #43
# Created: November 09, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################


# Given a signed 32-bit integer x, return x with its digits reversed. 
# If reversing x causes the value to go outside the signed 32-bit integer range [-2**31, 2**31 - 1], 
# then return 0.
# source: https://leetcode.com/problems/reverse-integer/


reverseInteger <- function(x){
  if ( -2**31 < x & x > 2**31 -1) return(0) #must be inside the integer boundaries
  if (x < 0) { 
    x2 <- x*-1
    r_ints <- (rev(strsplit(as.character(x2), "")[[1]]))
  } else {
    r_ints <- (rev(strsplit(as.character(x), "")[[1]]))
  }
  r_ints2 <- paste(r_ints, collapse = "")
  r_ints2 <- as.numeric(r_ints2)
  

  if ( -2**31 < r_ints2 & r_ints2 > 2**31 -1) {
      return(0) 
  } else {
  return(r_ints2)
 }
}

#################
#function check
################

reverseInteger(-4122310)
# [1] -132214
reverseInteger(122310)
# [1] 13221
reverseInteger(12223456789) # returns zero at the beginning
# [1] 0

reverseInteger(2147483646) # returns zero after reversing the integer
# [1] 0