
#############################
### Greedy algorithm for TSP (Traveling salesman problem)
#############################
library(ggplot2)
library(tidyverse)

#data
set.seed(2908)
cities <- data.frame(x = sample(rnorm(1,100,20),1000,replace=TRUE),
                     y = sample(rnorm(1,100,15),1000,replace=TRUE),
                     unvisited = 0,
                     rn = 1:1000)

# plot(cities$x~cities$y)


# helper function
nearest_neighbour <- function(pos, cities){
  id <- which.min(colSums((t(cities) - pos)^2))
  idx <- cities[id, ]
  return(idx)
}


#algorithm
greedy_algo <- function(cities){
  pos.ix = (cities[sample(1:nrow(cities), 1), ])
  tour <<- c(NULL)
  unvisited <<- cities[!(cities$rn %in% pos.ix$rn), ]
  while (unvisited$unvisited == 0) {
    C = nearest_neighbour(pos.ix, unvisited)
    tour <<- rbind(tour, C)
    
    unvisited <<- unvisited[!(unvisited$rn %in% C$rn), ]
    pos.ix <- tail(tour)
    #print(pos.ix)
    
  }
  return(tour)
}


#run greedy
greedy_algo(cities)

tourQ <- data.frame(x=tour$x,y=tour$y)

plot_path <- function(df){
  for (i in 1:nrow(df)){
    dt_temp <- head(df,i)
    p  <- ggplot(dt_temp, aes(x = dt_temp$x, y = dt_temp$y)) +
      geom_line() +
      geom_point()
    plot(p)
    Sys.sleep(0.2)
    i <- i + 1
  }

}

# solution
plot_path(tour)

