##########################################
# 
#         Small multiple graphs
#
# Series:
# Little Useless-useful R functions #28
#
# Created: September 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com

# Changelog: 
###########################################

set.seed(2908)

# 1. Making data of 20 cases
myData <- data.frame(value=rnorm(2000, mean = 40, sd=20))
cases <- NULL
for (i in 1:20) {
  cases[[i]] <- sample(myData$value, size=32)
}


# 2. Get some libs for plotting

# get libraries we need for plotting and stacking the plots
library(grid)
library(gridExtra)
library(ggplot2)


problemColors <- c("TRUE"="red", "FALSE"="darkgrey")
colorScale <- scale_colour_manual(name="problem", values=problemColors)


finalTop <- ggplot(data=estimates, aes(x=meanDiff, y=sampleNum)) +
  
  geom_errorbarh(aes(xmin=lower,xmax=upper, color=problem)) +
  geom_point(aes(color=problem))  + 
  geom_vline(xintercept = 10, color="blue") +
  scale_y_reverse() + 
  geom_text(aes(x=-24, y=sampleNum, 
                label=paste("Mean:", round(meanDiff,2))),
            size = 2.5, hjust="inward") +
  
  geom_text(aes(x=-17, y=sampleNum, 
                label=paste("p: ",
                            p, sep="")),
            size = 2.5, hjust="inward") +
  
  scale_x_continuous(limits=c(-26,30)) +
  colorScale +
  theme_void() +

  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.65),
        plot.margin = unit(c(10, 0, -2, 0), "pt")) +
  
  ggtitle("Mean, 95% CI") +
  geom_text(aes(x=-12, y=sampleNum, 
                label=estimates$significance),
            size = 2.5, hjust="inward") 


# 3. Final plotting

finalComplete <- grid.arrange(finalTop,ncol = 1, heights = c(4, 1))

