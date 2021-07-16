##########################################
# 
# Colourful line graph with ggplot
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


Colourful_graph <- function(n, x, y){
  df <- data.frame(x=x, y=y, col=n)
  for (i in 1:n){
    #get last x,y
    lastx <- tail(df$x, 1)
    lasty <- tail(df$y, 1)
    col <- sample(1:i, 1, replace = T)
    if (i %% 10 == 0) {
      xx <- runif(1, 0.0, 1.0) + lastx
      yy <- runif(1, 0.0, 1.0) - lasty
    } else  {
    xx <- runif(1, 0.0, 1.0) + lastx
    yy <- runif(1, 0.0, 1.0) - lasty
    }
    # change: col=i for rainbow colours
    df <- rbind(df, c(x=xx, y=yy, col=col)) 
    
  }
  fake <<- df
  # show faked graph
  ggplot(fake, aes(x, y, color = factor(col))) +
    geom_path(aes(group=1)) +
    theme(legend.position = "none") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(), text=element_blank(), line = element_blank())  
}



#create colourful graph
Colourful_graph(500,0.4,0.3)



