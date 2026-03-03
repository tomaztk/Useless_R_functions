##########################################
# 
# Useless R typing speed  benchmark 
#
# Series:
# Little Useless-useful R functions #91
# Created: March 03, 2026
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

###########################################

is_palindrome <- function(x) {
  chars <- strsplit(x, "")[[1]]
  identical(chars, rev(chars))
}

reverse_string <- function(x) {
  paste(rev(strsplit(x, "")[[1]]), collapse = "")
}

date_to_digits <- function(date, format = "%Y%m%d") {
  format(date, format)
}


date_formats <- list(
  YYYYMMDD = list(
    name = "YYYYMMDD",
    format = "%Y%m%d",
    example = "20251202",
    regions = "International/ISO standard"
  ),
  MMDDYYYY = list(
    name = "MMDDYYYY",
    format = "%m%d%Y",
    example = "12022025",
    regions = "United States"
  ),
  DDMMYYYY = list(
    name = "DDMMYYYY",
    format = "%d%m%Y",
    example = "02122025",
    regions = "Europe"
  ),
  YYMMDD = list(
    name = "YYMMDD",
    format = "%y%m%d",
    example = "251202",
    regions = ""
  ),
  DDMMYY = list(
    name = "DDMMYY",
    format = "%d%m%y",
    example = "021225",
    regions = ""
  ),
  MMDDYY = list(
    name = "MMDDYY",
    format = "%m%d%y",
    example = "120225",
    regions = "United states - short"
  )
)
