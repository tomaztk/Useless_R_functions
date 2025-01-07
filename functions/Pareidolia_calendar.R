#getwd()

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



### work in progress with dots:

library(ggplot2)
library(dplyr)
library(lubridate)


year <- 2025
month <- 3

days_in_month <- function(date) {
  as.numeric(format(date + months(1) - days(1), "%d"))
}


start_date <- as.Date(paste(year, month, "01", sep = "-"))
end_date <- as.Date(paste(year, month, days_in_month(start_date), sep = "-"))
days <- seq.Date(from = start_date, to = end_date, by = "day")

calendar <- data.frame(
  date = days,
  day = wday(days, label = TRUE, abbr = TRUE), # Day of the week (e.g., "Su", "Mo")
  week = as.numeric(format(days, "%U")) - as.numeric(format(start_date, "%U")) + 1, 
  day_num = day(days) 
)

head(calendar,3)  


face_patterns <- calendar %>%
  filter(day %in% c("Sun", "Mon", "Wed", "Fri")) %>%
  group_by(week) %>%
  summarise(
    face_detected = all(c("Sun", "Mon", "Wed", "Fri") %in% day),
    .groups = "drop"
  ) %>%
  filter(face_detected) %>%
  inner_join(calendar, by = "week") %>%
  filter(day %in% c("Sun", "Mon", "Wed", "Fri")) %>%
  mutate(pattern = "Face")

face_patterns

# Detect "Smile" patterns
smile_patterns <- calendar %>%
  filter(day %in% c("Tue", "Wed", "Thu", "Sat")) %>%
  group_by(week) %>%
  summarise(
    smile_detected = all(c("Tue", "Wed", "Thu", "Sat") %in% day),
    .groups = "drop"
  ) %>%
  filter(smile_detected) %>%
  inner_join(calendar, by = "week") %>%
  filter(day %in% c("Tue", "Wed", "Thu", "Sat")) %>%
  mutate(pattern = "Smile")

smile_patterns  



detected_patterns <- bind_rows(face_patterns, smile_patterns)
#  View(detected_patterns)

# Ensure the detected patterns have distinct colors
ggplot(calendar, aes(x = day, y = -week)) +
  geom_tile(aes(fill = date), color = "white", show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = day_num), size = 4, color = "black") +
  
  # Highlight detected patterns
  geom_point(
    data = detected_patterns,
    aes(x = day, y = -week, color = pattern),
    size = 5
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = paste("Calendar Pareidolia for", month, year),
    subtitle = "Detected patterns are highlighted in red (faces) and green (smiles)",
    x = "Day of the Week",
    y = ""
  )


## Just a calendar:


calendar_pareidolia <- function(year, month) {
  
  start_date <- as.Date(paste(year, month, "01", sep = "-"))
  end_date <- as.Date(paste(year, month, days_in_month(start_date), sep = "-"))
  days <- seq.Date(from = start_date, to = end_date, by = "day")
  
  
  calendar <- data.frame(
    date = days,
    day = wday(days, label = TRUE, abbr = TRUE), # Day of the week (e.g., "Mo", "Tu")
    week = as.numeric(format(days, "%U")) - as.numeric(format(start_date, "%U")) + 1,
    day_num = day(days)
  )
  
  # Define patterns (e.g., "eyes and mouth" for faces)
  patterns <- list(
    face = c("Su", "Mo", "We", "Fr", "Su"), # E.g: eyes (Su/Mo), nose (We), mouth (Fr)
    smile = c("Tu", "We", "Th"),            # Smile
    line = c("Mo", "Tu", "We", "Th", "Fr")  # Vertical line 
  )
  
  
  detected_patterns <- lapply(names(patterns), function(pattern_name) {
    pat <- patterns[[pattern_name]]
    calendar %>%
      group_by(week) %>%
      filter(day %in% pat) %>%
      mutate(pattern_name = pattern_name) %>%
      filter(n_distinct(day) == length(pat)) 
  }) %>%
    bind_rows() # ???
  
  detected_patterns <- detected_patterns %>% distinct()
  
  ggplot(calendar, aes(x = day, y = -week)) +
    geom_tile(aes(fill = date), color = "white", show.legend = FALSE) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    geom_text(aes(label = day_num), size = 4, color = "black") +
    
    geom_point( data = detected_patterns, aes(x = day, y = -week, color = pattern_name), size = 5 ) +
    scale_color_manual(
      values = c("face" = "red", "smile" = "green", "line" = "orange"),
      name = "Pattern"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      title = paste("Calendar Pareidolia for", month, year),
      x = "Day of the Week",
      y = ""
    )
}


days_in_month <- function(date) {
  as.numeric(format(date + months(1) - days(1), "%d"))
}

# Example
calendar_pareidolia(2025, 1)


## With actual curves :)

calendar_pareidolia <- function(year, month) {
  start_date <- as.Date(paste(year, month, "01", sep = "-"))
  end_date <- as.Date(paste(year, month, days_in_month(start_date), sep = "-"))
  days <- seq.Date(from = start_date, to = end_date, by = "day")
  
  calendar <- data.frame(
    date = days,
    day = wday(days, label = TRUE, abbr = TRUE), # Day of the week (e.g., "Su", "Mo")
    week = as.numeric(format(days, "%U")) - as.numeric(format(start_date, "%U")) + 1, # Week of the month
    day_num = day(days) # Numeric day of the month
  )
  
  face_patterns <- calendar %>%
    group_by(week) %>%
    filter(all(c("Sun", "Mon", "Wed", "Fri") %in% day)) %>%
    filter(day %in% c("Sun", "Mon", "Wed", "Fri")) %>%
    mutate(feature = ifelse(day %in% c("Sun", "Fri"), "eye", "smile")) %>%
    ungroup()
  
  detected_patterns <- face_patterns
  
  if (nrow(detected_patterns) == 0) {
    message("No patterns detected for the given month and year.")
    return(NULL)
  }
  
  # Visualization: Calendar with highlighted patterns
  ggplot(calendar, aes(x = day, y = -week)) +
    # Basic calendar grid
    geom_tile(aes(fill = date), color = "white", show.legend = FALSE) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    geom_text(aes(label = day_num), size = 4, color = "black") +
    # Add "eyes" as red circles
    geom_point(
      data = filter(detected_patterns, feature == "eye"),
      aes(x = day, y = -week),
      color = "red",
      size = 6
    ) +
    # Add "smile" as green arcs
    geom_curve(
      data = filter(detected_patterns, feature == "smile"),
      aes(x = as.numeric(day) - 0.3, xend = as.numeric(day) + 0.3,
          y = -week - 0.1, yend = -week - 0.1),
      curvature = -0.3,
      color = "green",
      size = 1
    ) +
    # Highlight feature types (eyes and smile)
    geom_text(
      data = detected_patterns,
      aes(label = feature, color = feature),
      vjust = 1.5,
      size = 4
    ) +
    scale_color_manual(values = c("eye" = "red", "smile" = "green")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    labs(
      title = paste("Calendar Pareidolia for", month, year),
      subtitle = "Eyes are red circles; smile is a green curve",
      x = "Day of the Week",
      y = ""
    )
}


days_in_month <- function(date) {
  as.numeric(format(date + months(1) - days(1), "%d"))
}

# Example 
calendar_pareidolia(2025, 1)

