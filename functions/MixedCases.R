#################################
# 
# Random Small Caps Letter 
# Created: October 15, 2020
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
############################### 

# number of consecutive trailing elements matching pattern
count_trailing_matches <- function(x, pattern) {
  # number of elements after the last non-matching element
  length(x) - max(c(0, grep(pattern, x, invert = TRUE)))
}
# count_trailing_matches(c("a", "a", "b", "b"), "a")
# count_trailing_matches(c("a", "a", "b", "b"), "b")
# count_trailing_matches(c("a", "a"), "a")
# count_trailing_matches(c("a", "a"), "b")
# count_trailing_matches(c("a", "a", "b", "a"), "a")
# count_trailing_matches(c("a", "a", "b", "a"), "b")

# linear decreasing weight from 1 (x <= min) to 0 (x >= max) 
linear_weight <- function(x, min, max) {
  # avoid pmax(pmin()) to allow e.g. min = Inf 
  weight <- ifelse(
    x <= min,
    1,
    ifelse(
      x >= max,
      0,
      (max - x) / (max - min)
    )
  )
  weight
}
# linear_weight(0:4, 1, 3)
# linear_weight(0:3, 1, 2)
# linear_weight(0:3, 0, 2)
# linear_weight(0:3, Inf, Inf)
# linear_weight(0:3, 0, 1)

# weight based on the number of consecutive trailing matches
pattern_weight <- function(x, pattern, min, max) {
  linear_weight(
    count_trailing_matches(x, pattern),
    min = min, 
    max = max
  )
}


# The case of each letter in `string` is determined in a probabilistic way by
# sampling between lower- and upper-case. The sampling weight depends on the
# number of preceding consecutive lower- and upper-case letters, and decreases
# linearly from 1 (up to `min` letters) to 0 (for `max` letters).
# In particular, up to `min` repetitions both alternatives are sampled with
# equal weights, whereas with `max` repetitions the wight is 0 hence the
# opposite case is always used. As a consequence, the maximum number of
# consecutive letters with the same case is `max`.
MixedCases <- function(string, min = 1L, max = 2L) {
  upper <- "[A-Z]"
  lower <- "[a-z]"
  transofmers <- list(toupper, tolower)
  weights <- c(upper = 1, lower = 1)
  
  chars <- strsplit(string, "")[[1]]
  for (i in seq_along(chars)) {
    # previous max characters (pointless to go beyond)
    prev <- tail(head(chars, i - 1), max)
    weights["upper"] <- pattern_weight(prev, upper, min = min, max = max)
    weights["lower"] <- pattern_weight(prev, lower, min = min, max = max)
    transform <- sample(transofmers, 1, prob = weights)[[1]]
    chars[[i]] <- transform(chars[[i]])
  }
  return(paste(chars, collapse = ""))
}

set.seed(12358)
MixedCases("This is useless R Function that seems to exists.")
# equal-weights, no more than 2 repetitions
set.seed(12358)
MixedCases(paste(rep("o", 100), collapse = ""), 1L, 2L)
# equal-weights, always (arbitrarily-long sequences allowed)
set.seed(12358)
MixedCases(paste(rep("o", 100), collapse = ""), Inf)
# equal-weights, no more than 3 repetitions
set.seed(12358)
MixedCases(paste(rep("o", 100), collapse = ""), 2L, 3L)
# alternating (no repetitions)
set.seed(12358)
MixedCases(paste(rep("o", 100), collapse = ""), 0L, 1L)
# max 2 repetitions, decreasing weight
set.seed(12358)
MixedCases(paste(rep("o", 100), collapse = ""), 0L, 2L)
