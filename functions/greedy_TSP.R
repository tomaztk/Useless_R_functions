
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
                     y = sample(rnorm(1,100,10),df_size,replace=TRUE),
                     rn = 1:df_size)


#plot(cities$y~cities$x)


# Absolute Nearest city function
nearest_city_absolute <- function(pos, cities){
  if (which.min(colSums((t(cities[,c("x","y")]) - pos)^2)) >= which.min(colSums((t(cities[,c("y","x")]) - pos)^2))) {
    id <- which.min(colSums((t(cities[,c("x","y")]) - pos)^2))
  } else {
    id <- which.min(colSums((t(cities[,c("y","x")]) - pos)^2))
  }
  return(cities[id, ])
}

# Nearest city function for coordinate system (where x and y must not be switched!)
nearest_city_coordinate <- function(pos, cities){
  id <- which.min(colSums((t(cities[,c("x","y")]) - pos)^2))
  return(cities[id, ])
}


# Greedy TSP algorithm
greedy_TSP <- function(cities){
  pos.ix <- (cities[sample(1:nrow(cities), 1), ])
  pos.ix.c <- c(pos.ix$y,pos.ix$x)
  tour <- pos.ix
  unvisited <- cities[!(cities$rn %in% pos.ix$rn), ]
  while (nrow(unvisited) > 0) {
    pos.ix <- tail(tour,1)
    pos.ix.c <- c(pos.ix$x, pos.ix$y)
    found <- nearest_city_coordinate(pos.ix.c, unvisited)
    tour <- rbind(tour, found)
    unvisited <- unvisited[!(unvisited$rn %in% found$rn), ]

  }
  tour$name <- as.character(seq.int(nrow(tour)))
  return(tour)
}


# Plot path
plot_path <- function(df){
  for (i in 1:nrow(df)){
    dt_temp <- head(df,i)
    #p2 <- ggplot() + geom_point(data=df, aes(x = x, y = y), colour="black", size = 1) +  geom_point(data=dt_temp, aes(x=x,y=y), colour="red", size = 1, label=dt_temp$name)
    #plot(p)     # each step
    #Sys.sleep(0.01)
  }
  p <- ggplot(data=dt_temp, aes(x = x, y = y, label=name) , colour="red", size = 2) +    geom_point() + geom_text(aes(label=name), hjust=1, vjust=-0.1) +theme(aspect.ratio=1) + coord_cartesian(xlim = c(0, 125), ylim = c(0, 125))
  plot(p)
}


#Run together
plot_path(greedy_TSP(cities))


##############
## Animation
##############
mm <- greedy_TSP(cities)
library(gganimate)
ggplot(data=mm, aes(x = x, y = y, label=name) , colour="red", size = 2) +
  geom_point(show.legend = FALSE) +
  labs(title = 'Step #n: {as.numeric(format(round(frame_time, 2), nsmall = 2))}', x= 'X-axis', y =  'Y-axis') +
  transition_time(as.numeric(name)) +
  guides(alpha = F) +
  shadow_mark(past = T, future=F, alpha=0.3, colour="red")
