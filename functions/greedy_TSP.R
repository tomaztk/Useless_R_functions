
##########################################
# 
# Greedy TSP algorithm 
# (TSP = Traveling Salesman Problem)
#
# Series:
# Little Useless-useful R functions #41
# Created: August 13, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(ggplot2)
library(tidyverse)



#generate fake data
df_size <- 100

set.seed(2908)
cities <- data.frame(x = sample(rnorm(1,100,20),df_size,replace=TRUE),
                     y = sample(rnorm(1,100,15),df_size,replace=TRUE),
                     rn = 1:df_size)



# Nearest neighbor function
nearest_neighbour <- function(pos, cities){
  id <- which.min(colSums((t(cities) - pos)^2))
  idx <- cities[id, ]
  return(idx)
}


# Greedy algorithm
greedy_algo <- function(cities){
  pos.ix = (cities[sample(1:nrow(cities), 1), ])
  pos.ix.c <- c(pos.ix$x,pos.ix$y)
  tour <<- c(NULL)
  unvisited <<- cities[!(cities$rn %in% pos.ix$rn), ]
  while (nrow(unvisited) > 0) {
    C = nearest_neighbour(pos.ix.c, unvisited)
    tour <<- rbind(tour, C)
    
    unvisited <<- unvisited[!(unvisited$rn %in% C$rn), ]
    pos.ix <- tail(tour,1)
    pos.ix.c <- c(pos.ix$x, pos.ix$y)
    
  }
  return(tour)
}


# Plot path
plot_path <- function(df){
  for (i in 1:nrow(df)){
    dt_temp <- head(df,i)
    p <<- ggplot() + geom_point(data=df, aes(x = x, y = y), colour="black", size = 1) +  geom_point(data=dt_temp, aes(x=x,y=y), colour="red", size = 1)
    #plot(p)
    #Sys.sleep(0.01)
  }
  plot(p)
}



#run greedy
greedy_algo(cities)

# plot  solution
plot_path(tour)

