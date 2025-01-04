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


calendar_pareidolia <- function(year, month) {

  days <- seq.Date(from = as.Date(paste(year, month, "01", sep = "-")),
                   to = as.Date(paste(year, month, days_in_month(as.Date(paste(year, month, "01", sep = "-"))), sep = "-")),
                   by = "day")
  calendar <- data.frame(
    date = days,
    day = wday(days, label = TRUE),
    week = as.numeric(format(days, "%U")) - min(as.numeric(format(days, "%U"))) + 1
  )
  
  # Define quazi-patterns (example: "eyes and mouth" for faces)
  patterns <- list(
    face = c("Su", "Mo", "We", "Fr", "Su"),
    smile = c("Tu", "We", "Th"),
    vertical_line = c("Mo", "Tu", "We", "Th", "Fr")
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
  
  
  ## Draw calendar
  ggplot(calendar, aes(x = day, y = week)) +
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
      subtitle = "Highlighted patterns are ""detected"" pareidolia-like shapes",
      x = "Day of the Week",
      y = "Week?"
    )
}

# helper?
days_in_month <- function(date) {
  return(as.numeric(format(date + months(1) - days(1), "%d")))
}


# Run the function for February 2025
calendar_pareidolia(2025, 2) 
