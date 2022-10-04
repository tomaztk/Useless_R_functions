###############
##
### Functions
###
###
### Stored as file Stats_fun.R 
### Envoke functions in any other file
### as library(Stats_fun)
###############

library(tidyverse)

groupsum <- function(df, group_vars, sum_vars){
  df %>% 
    group_by_at(vars(one_of(group_vars))) %>% 
    summarise_at(vars(one_of(sum_vars)), list(sum = sum, mean = mean))
}

# Usage:
#  groupsum(mtcars,  
#    c("carb", "vs"), 
#    c("cyl", "hp")) 
#
#  groupsum(mtcars,"cyl", "hp") 


sum_var <- function(df, var){
  summarise_at(df, vars(one_of(var)), list(sum = sum, mean = mean))
  #summarise_at(df, vars(one_of(var)), sum)
  
}


# Usage:
#  sum_var(mtcars, "cyl")
#
#  sum_var(mtcars,c("cyl", "hp"))


Spread <- function(x){
  max(x) - min(x) 
}


# Usage:
#  Spread(mtcars$cyl)
#


##### drawing and calling function from a function

create_brand <- function(cars_df) {
  
  brands <- sapply(
    strsplit(rownames(cars_df), ' '), 
    '[', 
    1
  )
  
  return (brands)
}
mean_by_variable <- function(df, agg_var, by_var) {
  
  aggregate_brand <- aggregate(
    df[,agg_var],
    by = list(df[,by_var]),
    FUN = mean
  )
  
  return (aggregate_brand)
  
}


plot_sorted_scatter <- function(cars_data, agg_var, by_var) {
  
  cars_data$brand <- create_brand(cars_data)
  
  # Create Aggregation
  agg_data <- mean_by_variable(cars_data, agg_var, by_var)
  
  # Sort 
  sort_order <- factor(
    agg_data[order(agg_data[,'x']),]$Group.1
  )
  
  ggplot(
    data = agg_data,
    aes(x=factor(Group.1, levels=sort_order), y=x, color='darkred')
  ) + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}
