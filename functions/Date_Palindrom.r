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
        display_results <-  results
        
        for (i in seq_len(nrow(display_results))) {
          row <- display_results[i, ]
          cat(sprintf("  %-12s  %-10s  %-12s  %s ↔ %s\n",
                      format(row$date, "%Y-%m-%d"),
                      substr(row$day_of_week, 1, 10),
                      row$format,
                      row$digits,
                      reverse_string(row$digits)))
        }
  
        cat(strrep("─", 60), "\n\n")
        
        if (length(check_formats) > 1) {
          #funky stuff
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




#### Palindrome dates distributed per decade for each date format :)


collect_palindromes <- function(start_date, end_date, formats = "all") {
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (start_date > end_date) { tmp <- start_date; start_date <- end_date; end_date <- tmp }
  
  check_formats <- if (identical(formats, "all")) names(date_formats) else
    intersect(formats, names(date_formats))
  
  # Pre-build the date sequence for speed
  all_dates <- seq(start_date, end_date, by = "day")
  
  rows <- lapply(check_formats, function(fmt) {
    digits_vec <- format(all_dates, date_formats[[fmt]]$format)
    is_pal     <- vapply(digits_vec, is_palindrome, logical(1))
    if (!any(is_pal)) return(NULL)
    data.frame(date   = all_dates[is_pal],
               format = fmt,
               digits = digits_vec[is_pal],
               stringsAsFactors = FALSE)
  })
  
  do.call(rbind, Filter(Negate(is.null), rows))
}

date_formats <- list(
  YYYYMMDD = list(name = "YYYYMMDD", format = "%Y%m%d",
                  example = "20251202", regions = "International/ISO standard"),
  MMDDYYYY = list(name = "MMDDYYYY", format = "%m%d%Y",
                  example = "12022025", regions = "United States"),
  DDMMYYYY = list(name = "DDMMYYYY", format = "%d%m%Y",
                  example = "02122025", regions = "Europe"),
  YYMMDD   = list(name = "YYMMDD",   format = "%y%m%d",
                  example = "251202",  regions = ""),
  DDMMYY   = list(name = "DDMMYY",   format = "%d%m%y",
                  example = "021225",  regions = ""),
  MMDDYY   = list(name = "MMDDYY",   format = "%m%d%y",
                  example = "120225",  regions = "United States - short")
)
is_palindrome <- function(x) {
  chars <- strsplit(x, "")[[1]]
  identical(chars, rev(chars))
}


raw <- collect_palindromes("1000-01-01", "2100-12-31", formats = "all")

raw <- raw %>%
  mutate(
    year   = as.integer(format(date, "%Y")),
    decade = floor(year / 10) * 10
  )


panel_meta <- list(
  list(fmts  = "YYYYMMDD",
       title = "YYYYMMDD — ISO / International",
       fill  = "#2196F3"),
  list(fmts  = "MMDDYYYY",
       title = "MMDDYYYY — United States",
       fill  = "#E91E63"),
  list(fmts  = "DDMMYYYY",
       title = "DDMMYYYY — Europe",
       fill  = "#4CAF50"),
  list(fmts  = c("YYMMDD", "DDMMYY", "MMDDYY"),
       title = "Short-year formats (YYMMDD / DDMMYY / MMDDYY)",
       fill  = "#FF9800")
)

make_panel <- function(meta, data) {
  
  sub <- data %>%
    filter(format %in% meta$fmts) %>%
    count(decade, format, name = "count")
  
  # Decade label  e.g. "1800s"
  sub$decade_label <- paste0(sub$decade, "s")
  
  # Keep all decades in range so x-axis is continuous
  all_decades <- data.frame(decade = seq(min(data$decade), max(data$decade), 10))
  all_decades$decade_label <- paste0(all_decades$decade, "s")
  
  p <- ggplot(sub, aes(x = decade, y = count)) +
    geom_col(fill = meta$fill, colour = "white", width = 8, alpha = .85) +
    scale_x_continuous(
      breaks = seq(min(data$decade), max(data$decade), 20),
      labels = function(x) paste0(x, "s"),
      expand = expansion(mult = .02)
    ) +
    labs(title    = meta$title,
         subtitle = sprintf("%d palindromes across %d decades",
                            sum(sub$count), n_distinct(sub$decade)),
         x = "Decade",
         y = "Count") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 11, colour = "#222222"),
      plot.subtitle   = element_text(size = 9,  colour = "#666666"),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y     = element_text(size = 8),
      axis.title      = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.margin     = margin(8, 12, 8, 8)
    )
  
  # If panel 4 (3 formats), add colour by sub-format
  if (length(meta$fmts) > 1) {
    p <- ggplot(sub, aes(x = decade, y = count, fill = format)) +
      geom_col(colour = "white", width = 8, alpha = .85, position = "stack") +
      scale_fill_manual(values = c(YYMMDD = "#FF9800", DDMMYY = "#FF5722", MMDDYY = "#FFC107"),
                        name = "Format") +
      scale_x_continuous(
        breaks = seq(min(data$decade), max(data$decade), 20),
        labels = function(x) paste0(x, "s"),
        expand = expansion(mult = .02)
      ) +
      labs(title    = meta$title,
           subtitle = sprintf("%d palindromes across %d decades",
                              sum(sub$count), n_distinct(sub$decade)),
           x = "Decade",
           y = "Count") +
      theme_minimal(base_size = 11) +
      theme(
        plot.title      = element_text(face = "bold", size = 11, colour = "#222222"),
        plot.subtitle   = element_text(size = 9,  colour = "#666666"),
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y     = element_text(size = 8),
        axis.title      = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.text     = element_text(size = 8),
        plot.margin     = margin(8, 12, 8, 8)
      )
  }
  
  p
}

library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)

panels <- lapply(panel_meta, make_panel, data = raw)

title_top <- textGrob(
  "Palindrome Dates by Decade  (1100 \u2013 2100)\nEach bar shows how many times a date reads identically forwards and backwards",
  gp = gpar(fontsize = 13, fontface = "bold", lineheight = 1.4)
)
caption_bot <- textGrob(
  "Formats: YYYYMMDD (ISO)  \u00b7  MMDDYYYY (US)  \u00b7  DDMMYYYY (EU)  \u00b7  Short-year family (YY\u2026)",
  gp = gpar(fontsize = 8, col = "#888888")
)


grid.arrange(
  panels[[1]], panels[[2]], panels[[3]], panels[[4]],
  ncol   = 2,
  top    = title_top,
  bottom = caption_bot
)
dev.off()

### results
raw %>%
  count(decade, format) %>%
  tidyr::pivot_wider(names_from = "format", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

