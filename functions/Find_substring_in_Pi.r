##########################################
# 
# Finding substrings of Pi number
#
# Series:
# Little Useless-useful R functions #82
# Created: November  10, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

###########################################


library(Rmpfr)

## A->1 ... Z->26 + removes spaces/punct, case-insensitive.
encode_a1z26 <- function(s) {
  s_clean <- gsub("[^A-Za-z]", "", toupper(s))
  if (nchar(s_clean) == 0) stop("No letters found in input.")
  letters_vec <- strsplit(s_clean, "", fixed = TRUE)[[1]]
  nums <- match(letters_vec, LETTERS)          
  paste0(nums, collapse = "")
}


# Compute π to n-digits
pi_fraction_digits <- function(n_digits) {
  # bits of precison ~ n_digits * log2(10)
  precBits <- ceiling(n_digits * log2(10)) + 32L
  pi_mpfr <- Const("pi", precBits)                        
  s <- formatMpfr(pi_mpfr, digits = n_digits + 2L, scientific = FALSE, base = 10L)
  s <- gsub("\\.", "", s, fixed = FALSE)
  frac <- substr(s, 2L, n_digits + 1L)
  if (nchar(frac) < n_digits) {
    stop("To low precision; did not get requested number of digits")
  }
  frac
}


find_in_pi <- function(pattern, n_digits) {
  if (!grepl("^[0-9]+$", pattern)) stop("Pattern must be digits!")
  t0 <- proc.time()[["elapsed"]]
  frac <- pi_fraction_digits(n_digits)
  loc <- regexpr(pattern, frac, fixed = TRUE)
  elapsed <- proc.time()[["elapsed"]] - t0
  
  #getting the positions
  if (loc[1] != -1) {
    start_pos <- as.integer(loc[1])                
    end_pos   <- start_pos + nchar(pattern) - 1L
    list(found = TRUE,
         start = start_pos,
         end   = end_pos,
         digits_scanned = n_digits,
         seconds = elapsed)
  } else {
    list(found = FALSE,
         start = NA_integer_,
         end   = NA_integer_,
         digits_scanned = n_digits,
         seconds = elapsed)
  }
}

# 4) Convenience function: phrase -> A1Z26 -> search in π
find_phrase_in_pi <- function(phrase, n_digits) {
  pat <- encode_a1z26(phrase)
  res <- find_in_pi(pat, n_digits)
  res$pattern <- pat
  res$phrase  <- phrase
  res
}


## Run functions
word <- "eggs"
encoded_word <- encode_a1z26(word)
cat("Encoded ",word," ->", encoded_word, "\n")  
result_today <- find_in_pi(encoded_word, 1e5)
print(result_today)


 