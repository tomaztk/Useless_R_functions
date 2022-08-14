
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
df_size <- 50

set.seed(2908)
cities <- data.frame(x = sample(rnorm(1,100,10),df_size,replace=TRUE),
                     y = sample(rnorm(1,100,10),df_size,replace=TRUE)
                     ,
                     rn = 1:df_size)


# Abosolute Nearest neighbor function
nearest_neighbour_absolute <- function(pos, cities){
  if (which.min(colSums((t(cities[,c("x","y")]) - pos)^2)) >= which.min(colSums((t(cities[,c("y","x")]) - pos)^2))) {
    id <- which.min(colSums((t(cities[,c("x","y")]) - pos)^2))
  } else {
    id <- which.min(colSums((t(cities[,c("y","x")]) - pos)^2))
  }
  
  idx <- cities[id, ]
  return(idx)
}

# Nearest neighbour function for coordinate system
nearest_neighbour_coordinate <- function(pos, cities){
  id <- which.min(colSums((t(cities[,c("x","y")]) - pos)^2))
  idx <- cities[id, ]
  return(idx)
}


# Greedy algorithm
greedy_algo <- function(cities){
  pos.ix <- (cities[sample(1:nrow(cities), 1), ])
  pos.ix.c <- c(pos.ix$y,pos.ix$x)
  tour <- c(NULL)
  tour <- pos.ix
  unvisited <- cities[!(cities$rn %in% pos.ix$rn), ]
  while (nrow(unvisited) > 0) {
    pos.ix <- tail(tour,1)
    pos.ix.c <- c(pos.ix$x, pos.ix$y)
    C <- nearest_neighbour_coordinate(pos.ix.c, unvisited)
    tour <- rbind(tour, C)
    unvisited <- unvisited[!(unvisited$rn %in% C$rn), ]

  }
  tour$name <- as.character(seq.int(nrow(tour)))
  return(tour)
}

# Plot path
plot_path <- function(df){
  for (i in 1:nrow(df)){
    dt_temp <- head(df,i)
    p2 <- ggplot() + geom_point(data=df, aes(x = x, y = y), colour="black", size = 1) +  geom_point(data=dt_temp, aes(x=x,y=y), colour="red", size = 1, label=dt_temp$name)
    #plot(p)     # each step
    #Sys.sleep(0.01)
  }
  p <- ggplot(data=dt_temp, aes(x = x, y = y, label=name) , colour="red", size = 2) +    geom_point() + geom_text(aes(label=name), hjust=1, vjust=-0.1) +theme(aspect.ratio=1) + coord_cartesian(xlim = c(0, 125), ylim = c(0, 125))
  plot(p)
}

#run greedy
#greedy_algo(cities)

# plot  solution
#plot_path(tour)

#Run together
plot_path(greedy_algo(cities))
