
#############################
### Greedy algorithm for TSP (Traveling salesman problem)
#############################

#data
set.seed(2908)
cities <- data.frame(x = sample(rnorm(1,100,20),1000,replace=TRUE),
                     y = sample(rnorm(1,100,15),1000,replace=TRUE),
                     unvisited = 0,
                     order = 0)
plot(cities$x~cities$y)


#algorithm
greedy_algo <- function(cities){
  C = (cities[sample(1:nrow(cities), 1), ])
  tour = c(NULL)
  unvisited = cities[which(cities$unvisited == 0),]
  while (cities$unvisited == 0) {
    C = nearest_neighbour(C, unvisited)
    tour <- rbind(tour, C)
    cities$unvisited <- 1
  }
  return(tour)
  
}

nearest_neighbour <- function(A, cities){
  return(min(lapply(cities, function(x) distance(C,A))))
  #return(min(cities, key=lambda C: distance_points(C,A)))
}

#first <- function(df){
#  return (df[sample(1:nrow(df), 1), ])
#}



distance_points <- function(C,A){
  
}
