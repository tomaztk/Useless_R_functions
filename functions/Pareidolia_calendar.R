getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")

library(ggplot2)
library(dplyr)
library(lubridate)


##########################################
# 
# Function t0 generate calendar pareidolia
#
# Series:
# Little Useless-useful R functions #65
# Created: January 03, 2035
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


days_in_month <- function(date) {
  return(as.numeric(format(date + months(1) - days(1), "%d")))
}

# 
# year <- 2025
# month <- 2
# as.Date(paste(year, month, "01", sep = "-"))
# as.Date(paste(year, month, days_in_month(as.Date(paste(year, month, "01", sep = "-"))), sep = "-"))
# 
# 
# days <- seq.Date(from = as.Date(paste(year, month, "01", sep = "-")),
#                  to = as.Date(paste(year, month, days_in_month(as.Date(paste(year, month, "01", sep = "-"))), sep = "-")),
#                  by = "day")
# 
# calendar <- data.frame(
#   date = days,
#   day = wday(days, label = TRUE, locale = Sys.getlocale("LC_TIME") ),
#   #week = as.numeric(format(days, "%U")) - min(as.numeric(format(days, "%U"))) + 1
#   week = lubridate::week(days)
# )
# 
# calendar
# lubridate::week("2025-02-28")
# 
# wday(x, label = FALSE, abbr = TRUE, week_start = getOption("lubridate.week.start", 7), locale = Sys.getlocale("LC_TIME"))

calendar_pareidolia <- function(year, month) {

  days <- seq.Date(from = as.Date(paste(year, month, "01", sep = "-")),
                   to = as.Date(paste(year, month, days_in_month(as.Date(paste(year, month, "01", sep = "-"))), sep = "-")),
                   by = "day")
  calendar <- data.frame(
    date = days,
    day = wday(days, label = TRUE),
    week = as.numeric(format(days, "%U")) - min(as.numeric(format(days, "%U"))) + 1,
    week_graph = lubridate::week(days)
  )
  head(calendar,3)
  
  # Define quazi-patterns (example: "eyes and mouth" for faces)
  patterns <- list(
    face = c("Sun", "Mon", "Wed", "Fri", "Sun"),
    smile = c("Tue", "Wed", "Thu", "Sat"),
    vertical_line = c("Mon", "Tue", "Wed", "Thu", "Fri")
  )
  
  # "detect" quazi- patterns
  detected_patterns <- lapply(patterns, function(pat) {
    calendar %>%
      group_by(week) %>%
      filter(day %in% pat) %>%
      summarise(
        match = ifelse(all(pat %in% day), TRUE, FALSE),
        pattern = paste(pat, collapse = ", ")
      ) %>%
      filter(match == TRUE)
  })
  
  detected_patterns <- bind_rows(detected_patterns, .id = "pattern_name")
  View(detected_patterns)
  
  ## Draw calendar
  ggplot(calendar, aes(x = day, y = week_graph)) +
    geom_tile(aes(fill = date), color = "white", show.legend = FALSE) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    geom_text(aes(label = day(date)), size = 4, color = "black") +
    geom_point(
      data = calendar %>% filter(date %in% detected_patterns$date),
      aes(x = day, y = week),
      color = "red", size = 4
    ) +
    theme_minimal() +
    labs(
      title = paste("Calendar Pareidolia for", month, year),
      subtitle = "Highlighted patterns are 'detected' pareidolia-like shapes",
      x = "Day of the Week",
      y = "Week?"
    )
}

# helper?
days_in_month <- function(date) {
  return(as.numeric(format(date + months(1) - days(1), "%d")))
}

# Run the function for February 2025
calendar_pareidolia(2025, 1) 

