##########################################
# 
# ggplot Heatmap that looks like Github 
# contribution chart
#
# Series:
# Little Useless-useful R functions #42
# Created: August 27, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#        
###########################################


# Library
library(ggplot2)

colours <- c("#ebedf0", "#9be9a8", "#40c463","#309b4c","#216e39")
# colours: lavender, palegreen, mediumseagreen,seagreen, forestgreen
# nof_contributions:  0, 1, 2-5, 5-10, 10+


# Generate grid
y <- weekdays(Sys.Date()+0:6) # days
x <- paste0("W_", seq(1,52))
data <- expand.grid(X=x, Y=y)
data$cols <- sample(colours, 364, replace=TRUE)


# Contribution graph 
ggplot(data, aes(X, Y)) + 
    geom_tile(aes(fill = cols, width=0.9, height=0.9)) + 
    scale_fill_manual(values=colours) + 
  labs(title = "Your R generated contributions in past year") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
       axis.ticks.x=element_blank(),
       axis.ticks.y=element_blank(),
       legend.position = "none",
       panel.background = element_rect(fill = 'white', color = 'white')
       )



##################
# square graph
# remember your local pool :-)
##################

# Generate grid
colours <- c("#ebedf0", "#9be9a8", "#40c463","#309b4c","#216e39")
y <- LETTERS[1:60] # letters
x <- seq(1,60)
data <- expand.grid(X=x, Y=y)
data$cols <- sample(colours, 3600, replace=TRUE)

ggplot(data, aes(X, Y)) + 
  geom_tile(aes(fill = cols, width=0.95, height=0.95)) + 
  scale_fill_manual(values=colours) + 
  labs(title = "Your local pool tiles") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', color = 'white')
  )
