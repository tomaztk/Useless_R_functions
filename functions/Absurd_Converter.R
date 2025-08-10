##########################################
# 
# Absurd converter with statistical 
# test and scientific report :-)
#
# Series:
# Little Useless-useful R functions #68
# Created: May  31, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

###########################################


library(ggplot2)
library(gganimate)

unit_converter_confuser <- function(values, unit_from = "kilograms", chaos = 0.3,
                                    seed = 42, report_path = "confuser_report.md") {
  set.seed(seed)
  
  # Absurdity of useful and useless mappings
  base_unit_map <- list(
    "meters"     = list(to = "lightyears", factor = 1.057e-16),
    "grams"      = list(to = "elephants", factor = 1 / 5000000),
    "liters"     = list(to = "bathtubs", factor = 1 / 150),
    "seconds"    = list(to = "centuries", factor = 1 / (60*60*24*365.25*100)),
    "newtons"    = list(to = "bananas", factor = 1 / 1.2),
    "calories"   = list(to = "joules", factor = 4.184),
    "degreesC"   = list(to = "ice creams melted", factor = 1 / 3),
    "meters"     = list(to = "smoots", factor = 1 / 1.7018),
    "grams"      = list(to = "blue whales", factor = 1 / 150000000),
    "liters"     = list(to = "coffee cups", factor = 1 / 0.24),
    "seconds"    = list(to = "Netflix episodes", factor = 1 / 1500),
    "newtons"    = list(to = "apples", factor = 1 / 1),
    "calories"   = list(to = "gummy bears", factor = 1 / 9),
    "degreesC"   = list(to = "angry chefs", factor = 1 / 45),
    "tweets"     = list(to = "Elon Musks", factor = 1 / 30000),
    "bananas"    = list(to = "Eiffel Towers", factor = 1 / 400000),
    "bitcoins"   = list(to = "pizzas (2010)", factor = 1 / 10000),
    "kilograms"  = list(to = "llamas", factor = 1 / 140),
    "decibels"   = list(to = "baby screams", factor = 1 / 15),
    "lumens"     = list(to = "fireflies", factor = 1 / 0.25),
    "hours"      = list(to = "meetings endured", factor = 1),
    "euros"      = list(to = "Swedish meatballs", factor = 1 / 1.5)
  )
  
  if (!(unit_from %in% names(base_unit_map))) stop("Unsupported unit_from")
  
  chaos_noise <- function(x) x * runif(1, 1 - chaos, 1 + chaos)
  
  base <- base_unit_map[[unit_from]]
  factor <- chaos_noise(base$factor)
  
  unit_to <- if (chaos > 0.5) sample(unlist(lapply(base_unit_map, `[[`, "to")), 1) else base$to
  unit_from_scramble <- if (chaos > 0.5) paste(sample(strsplit(unit_from, "")[[1]]), collapse = "") else unit_from
  
  if (chaos > 0.7) {
    values <- values + rnorm(length(values), mean = 0, sd = sd(values) * chaos)
  }
  
  converted <- values * factor
  
  df <- data.frame(
    value = c(values, converted),
    unit = rep(c(unit_from_scramble, unit_to), each = length(values)),
    time = rep(1:length(values), times = 2)
  )
  
  test <- if (chaos == 1) list(statistic = rnorm(1, 0, 10), p.value = runif(1)) else t.test(values, converted)
  

  p <- ggplot(df, aes(x = unit, y = value, fill = unit)) +
    geom_boxplot(alpha = 0.6) +
    geom_jitter(width = 0.15, size = 2, alpha = 0.8) +
    labs(
      title = "Wacky unit_converter_confuser()",
      subtitle = paste("Converting", unit_from, "→", unit_to),
      x = "Unit Type", y = "Value"
    ) +
    theme_minimal(base_size = 14) +
    transition_states(time, transition_length = 2, state_length = 1) +
    ease_aes("cubic-in-out")
  
  anim <<- animate(p, nframes = 50, fps = 10, width = 700, height = 500)
  
  
  # Markdown wacky parody Report Generation
  report <- paste0(
    "# Parody Scientific Report\n",
    "## Project: Wacky **unit_converter_confuser()**\n\n",
    "### Conversion: `", unit_from, "` ️-> `", unit_to, "`\n\n",
    "**Sample size**: ", length(values), "\n",
    "**Conversion factor**: ", signif(factor, 4), "\n\n",
    "### T-Test Results:\n",
    "- t-statistic: ", round(test$statistic, 3), "\n",
    "- p-value: ", format.pval(test$p.value, digits = 3), "\n",
    "- Conclusion: ", ifelse(test$p.value < 0.05, "Statistically significant nonsense!", "Not even the nonsense is significant."), "\n\n",
    "###  Plot included in animation.\n\n",
    "_This report is proudly brought to you by the Society of Confused Analysts._"
  )
  
  writeLines(report, report_path)
  message("Report saved to: ", report_path)
  
  return(invisible(list(
    original = data.frame(value = values, unit = unit_from),
    converted = data.frame(value = converted, unit = unit_to),
    t_test = test,
    report_path = report_path
  )))
}


set.seed(2908)
fake_weights <- rnorm(20, mean = 70, sd = 15)
unit_converter_confuser(fake_weights, unit_from = "kilograms", chaos = 0.92)

#check animation
anim

#check the report on your machine
