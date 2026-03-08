##########################################
# 
# Useless R typing speed  benchmark 
#
# Series:
# Little Useless-useful R functions #91
# Created: March 03, 2026
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.1

###########################################


## general function
IsPalindromeDate <- function(date = Sys.Date(),
                             formats = "all",
                             verbose = TRUE) {


  ## Helper stuff
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
  
  
  date <- as.Date(date)

  # Select formats
  if (identical(formats, "all")) {
    check_formats <- names(date_formats)
  } else {
    check_formats <- formats
    invalid <- setdiff(formats, names(date_formats))
    if (length(invalid) > 0) {
      warning("Unknown formats ignored: ", paste(invalid, collapse = ", "))
      check_formats <- intersect(formats, names(date_formats))
    }
  }

  results <- sapply(check_formats, function(fmt) {
    digits <- date_to_digits(date, date_formats[[fmt]]$format)
    is_palindrome(digits)
  })


  if (verbose) {
    any_palindrome <- FALSE

    for (fmt in check_formats) {
      digits <- date_to_digits(date, date_formats[[fmt]]$format)
      is_pal <- results[fmt]

      status <- if (is_pal) "Is palindrome" else "Not a palindrome!"
      if (is_pal) any_palindrome <- TRUE

      cat(sprintf("  %s %-15s : %s  %s\n",
                  date_formats[[fmt]]$name,
                  digits,
                  if (is_pal) "" else " ",
                  status))
    }

  }
}


## Usage examples
IsPalindromeDate()
IsPalindromeDate(as.Date("2020-02-02"))
IsPalindromeDate(as.Date("2021-12-02"), formats = "YYYYMMDD")


#########################
#### Function for range
########################

IsPalindromeDateRange <- function(date = Sys.Date(), 
                             end_date = NULL,
                             formats = "all",
                             verbose = TRUE) {
  
  
  
  ## Helper stuff
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
  
  
  date <- as.Date(date)
   
  if (identical(formats, "all")) {
    check_formats <- names(date_formats)
  } else {
    check_formats <- formats
    invalid <- setdiff(formats, names(date_formats))
    if (length(invalid) > 0) {
      warning("Unknown formats ignored: ", paste(invalid, collapse = ", "))
      check_formats <- intersect(formats, names(date_formats))
    }
  }
  
  if (!is.null(end_date)) {
    
    end_date <- as.Date(end_date)
    
    # Hearlthy checker start < end
    if (date > end_date) {
      temp <- date
      date <- end_date
      end_date <- temp
    }
    
    total_days <- as.integer(end_date - date) + 1
    
    if (verbose) {
      cat("From:", format(date, "%Y-%m-%d"))
      cat("To:  ", format(end_date, "%Y-%m-%d"))
      cat("(", total_days, " days)", sep = "")
      cat("\n")
      cat("  Formats: ", paste(check_formats, collapse = ", "), "\n\n", sep = "")
    }
    
    # Emopyt data frame
    results <- data.frame(
      date = as.Date(character()),
      day_of_week = character(),
      format = character(),
      digits = character(),
      stringsAsFactors = FALSE
    )
    
 
    check_date <- date
    checked <- 0
    
    while (check_date <= end_date) {
      
      for (fmt in check_formats) {
        digits <- date_to_digits(check_date, date_formats[[fmt]]$format)
        
        if (is_palindrome(digits)) {
          results <- rbind(results, data.frame(
            date = check_date,
            day_of_week = format(check_date, "%A"),
            format = fmt,
            digits = digits,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      check_date <- check_date + 1
      checked <- checked + 1
    }
    
    
    # Results
    if (verbose) {
      if (nrow(results) == 0) {
        cat("No date palindrome  found in this range.\n\n")
      } else {
        # Get uniques per date format
        unique_dates <- unique(results$date)
        
        cat(sprintf("Found %d palindrome occurrence%s (%d unique date%s):\n\n",
                    nrow(results),
                    if (nrow(results) > 1) "s" else "",
                    length(unique_dates),
                    if (length(unique_dates) > 1) "s" else ""))
        
        cat(strrep("─", 60), "\n")
        cat(sprintf("  %-12s  %-10s  %-12s  %s\n","Date", "Day", "Format", "Digits"))
        cat(strrep("─", 60), "\n")
        display_results <- if (nrow(results) > 50) head(results, 50) else results
        
        for (i in seq_len(nrow(display_results))) {
          row <- display_results[i, ]
          cat(sprintf("  %-12s  %-10s  %-12s  %s ↔ %s\n",
                      format(row$date, "%Y-%m-%d"),
                      substr(row$day_of_week, 1, 10),
                      row$format,
                      row$digits,
                      reverse_string(row$digits)))
        }
        
        if (nrow(results) > 50) {
          cat(sprintf("\n  ... and %d more (showing first 50)\n", nrow(results) - 50))
        }
        
        cat(strrep("─", 60), "\n\n")
        
        if (length(check_formats) > 1) {
          cat("Summary by format:\n")
          format_counts <- table(results$format)
          for (fmt in names(format_counts)) {
            cat(sprintf("%s: %d palindrome%s\n", fmt, format_counts[fmt],
                  if (format_counts[fmt] > 1) "s" else ""))
          }
          cat("\n")
        }
        
      }
    }
  }
  
  # Sanity Check 
  results <- sapply(check_formats, function(fmt) {
    digits <- date_to_digits(date, date_formats[[fmt]]$format)
    is_palindrome(digits)
  })
  
  if (verbose) {
    any_palindrome <- FALSE
    
    for (fmt in check_formats) {
      digits <- date_to_digits(date, date_formats[[fmt]]$format)
      is_pal <- results[fmt]
      
      status <- if (is_pal) "PALINDROME!" else "Not a palindrome"
      
      if (is_pal) any_palindrome <- TRUE
      
      cat(sprintf("  %s %-15s : %s  %s\n",
                  date_formats[[fmt]]$name,
                  digits,
                  if (is_pal) "<>" else " ",
                  status))
    }
    cat("\n")
  }
}


###
# Function usage - Range
###
IsPalindromeDateRange("2020-01-01", "2030-12-31")
IsPalindromeDateRange("2000-01-01", "2030-01-01", formats = "MMDDYYYY")

IsPalindromeDateRange("1100-01-01", "1200-01-01", formats = "DDMMYYYY")
