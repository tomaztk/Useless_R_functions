##########################################
# 
# Colorful graphs with ggplot
#
# Series:
# Little Useless-useful R functions #27
# Created: July 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

library(ggplot2)


## generating fake dataset (x,y) points for a single line
set.seed(208)


fake_data <- function(n, x, y){
  df <- data.frame(x=x, y=y)
  for (i in 1:n){
    #get last x,y
    lastx <- tail(df$x, 1)
    lasty <- tail(df$y, 1)
    if (i %% 10 == 0) {
      xx <- runif(1, 0.0, 1.0) + lastx
      yy <- runif(1, 0.0, 1.0) - lasty
    } else  {
    xx <- runif(1, 0.0, 1.0) + lastx
    yy <- runif(1, 0.0, 1.0) - lasty
      }
    df <- rbind(df, c(x=xx, y=yy))
    
  }
  fake <<- df
}

#create get faked
fake_data(500,0.4,0.3)


# show faked graph
ggplot(fake, aes(x, y, color = factor(x*y))) +
  geom_path(aes(group=1)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_blank(), text=element_blank(), line = element_blank())  

