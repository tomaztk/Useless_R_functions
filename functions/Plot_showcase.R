##########################################
# 
#       Showcase of base plot function
#
# Series:
# Little Useless-useful R functions #29
#
# Created: October 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com

# Change log: 
  
###########################################

library(magick)


#text = paste("\n   The following is text that'll appear in a plot window.\n",
#             "       As you can see, it's in the plot window",
#             "       One might imagine useful informaiton here")


# clean
rm(list = ls(all.names = TRUE))
dev.off(dev.list()["RStudioGD"])
graphics.off()
#gc() 



# create a temporary directory to store plot files
unlink(dir_out, recursive=TRUE)
dir_out <- file.path(tempdir(), "ShowCaseTempFolder")
dir.create(dir_out, recursive = TRUE)

set.seed(2908)


AllData <- data.frame(graph=c
                      (
                        "plot(ScatterData, main = 'Scatterplot')"
                        ,"plot(BarData, main = 'Histogram')"
                        ,"plot(BarData, rnorm(150), main = 'Boxplot')"
                        ,"plot(TimeSeriesData, main = 'Time-series')"
                        ,"plot(fun, -10, 5*pi, main = 'Plot a function')"
                        ,"plot(IrisData, main = 'Correlation plot for two variables')"
                        ,"plot(IrisData, main = 'Correlation plot for two \n  variables with lines of SS') +  
                           lines(lowess(iris[,1:2]))"
                        ,"plot(iris[, 1:3], main = 'Correlation plot for three or more')"
                        
                      ),
                      data=c('ScatterData <- cbind(rnorm(200),rnorm(200) * rnorm(200) + rnorm(200))'
                             ,'BarData <- factor(iris$Sepal.Width)'
                             ,'BarData <- factor(iris$Sepal.Width)'
                             ,'TimeSeriesData <- ts(matrix(rnorm(300), nrow = 300, ncol = 1), start = c(1990, 1), frequency = 12)'
                             ,'fun <- function(x) {x^4*pi}'
                             ,'IrisData <- as.data.frame(iris[, 1:2])'
                             ,'IrisData <- as.data.frame(iris[, 1:2])'
                             ,'iris'
                      )
)                
                      



for (i in 1:nrow(AllData)) {
  graphinfo  <- as.character(AllData$graph[i])
  datainfo <- as.character(AllData$data[i])
  eval(parse(text=datainfo))
  name_p <- paste0(dir_out,'\\',i,'.png')
  png(name_p)
  par(mfrow=c(3,1)) 
  p <- eval(parse(text=graphinfo))
  
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(graphinfo), cex = 1.5, col = "Darkgreen", font=1, adj=0.5)
  
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(datainfo), cex = 1.5, col = "Darkblue", font=1, adj=0)
  
  par(mfrow=c(1,1)) 
  dev.off()
}


# Render animation and store to disk
plot_animation <- image_animate(image_join(lapply(list.files(dir_out, full.names = TRUE), image_read)), fps = 0.5)
image_write(image = plot_animation,path = "C:\\Users\\Tomaz\\Desktop\\ShowCase.gif")
unlink(dir_out, recursive=TRUE)




