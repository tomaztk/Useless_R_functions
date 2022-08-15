
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


# or
greedy_TSP(cities) %>%
  select (x, y, name) %>%
  mutate(time_name=as.numeric(name)) %>%
  uncount(df_size, .id = "frame") %>%
  filter(time_name <= frame) %>%
  arrange(frame, time_name) %>%
  group_by(frame) %>%
  mutate(x_lag = lag(x), 
         y_lag = lag(y),
         tail = last(time_name) - time_name,
         point_alpha = if_else(tail == 0, 1, 0.3),
         segment_alpha = pmax(0, (df_size-tail)/df_size)) %>%
  ungroup() %>%
  ggplot(aes(x=y, y=x, xend = y_lag, yend = x_lag, group = time_name)) +
  geom_segment(aes(alpha = segment_alpha)) +
  geom_point(aes(alpha = point_alpha, colour="red"), show.legend = FALSE) +
  labs(title = 'Greedy Salesman travelling between the cities', x= 'X-axis', y =  'Y-axis') +
  scale_alpha(range = c(0,1)) +
  guides(alpha = F) +
  transition_manual(frame) 


