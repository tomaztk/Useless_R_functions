##########################################
# 
# Fermi Calculator
# 
#
# Series:
# Little Useless-useful R functions #64
# Created: April 06, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(ggplot2)
library(dplyr)

fermi_calculator <- function(problem = "piano_tuners_chicago", simulations = 1000) {
  
  if (problem == "piano_tuners_chicago") {
    
    pop_chicago <- rnorm(simulations, mean = 2.7e6, sd = 2e5) 
    households <- pop_chicago / rnorm(simulations, mean = 2.5, sd = 0.2)  # ppl per house
    pianos_per_household <- rnorm(simulations, mean = 0.02, sd = 0.005)  # fraction of pianos per house
    pianos_tuned_per_year <- rnorm(simulations, mean = 1, sd = 0.2)  # freq of piano tunning
    tunings_per_tuner <- rnorm(simulations, mean = 1000, sd = 200)  
    
    num_pianos <- households * pianos_per_household
    total_tunings <- num_pianos * pianos_tuned_per_year
    num_tuners <- total_tunings / tunings_per_tuner
    

    data <- data.frame(NumTuners = num_tuners)
    p <- ggplot(data, aes(x = NumTuners)) +
      geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
      geom_vline(xintercept = mean(num_tuners), color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Fermi Estimate: Number of Piano Tuners in Chicago",
           subtitle = "Using Monte Carlo Simulation",
           x = "Estimated Number of Tuners",
           y = "Frequency")
    
    print(p)
    
    return(summary(num_tuners))
  }
  
  else if (problem == "coffee_consumption_ljubljana") {
    pop_lj <- rnorm(simulations, mean = 3.6e5, sd = 2e4)  # citizens of LJ
    fraction_coffee_drinkers <- rnorm(simulations, mean = 0.65, sd = 0.05)  
    cups_per_day <- rnorm(simulations, mean = 2.5, sd = 0.5)  
    cup_size_liters <- rnorm(simulations, mean = 0.18, sd = 0.02)  # Size of a cup in liters
    
    daily_coffee_liters <- pop_lj * fraction_coffee_drinkers * cups_per_day * cup_size_liters
    data <- data.frame(DailyCoffeeLiters = daily_coffee_liters)
    
    p <- ggplot(data, aes(x = DailyCoffeeLiters)) +
      geom_histogram(fill = "brown", color = "black", bins = 30, alpha = 0.7) +
      geom_vline(xintercept = mean(daily_coffee_liters), color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Fermi Estimate: Coffee Consumption in Ljubljana",
           subtitle = "Using Monte Carlo Simulation",
           x = "Liters of Coffee Consumed per Day",
           y = "Frequency")
    
    print(p)
    return(summary(daily_coffee_liters))
  }
  
  else {
    stop("Problem not recognized. Try 'piano_tuners_chicago' or 'coffee_consumption_ljubljana'.")
  }
}


#################
# function usage
#################

fermi_calculator("piano_tuners_chicago")
fermi_calculator("coffee_consumption_ljubljana")

fermi_calculator("drunks_in_dublin")
