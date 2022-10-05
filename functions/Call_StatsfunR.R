pcks_4_install <- c("ggplot2", "leaflet", "plotly", "tidyverse")
install.packages(pcks_4_install, dependencies = TRUE)
lapply(pcks_4_install, library, character.only = TRUE)



getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")

source("Stats_fun.R")


groupsum(mtcars,"cyl", "hp") 


plot_sorted_scatter(mtcars, 'hp', 'brand')
