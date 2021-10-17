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

# clean environment
rm(list = ls(all.names = TRUE))
dev.off(dev.list()["RStudioGD"])
graphics.off()


library(magick)

set.seed(2908)


plot_animation <- function(SavePath){
    
      # create a temporary directory to store plot files
      dir_out <- file.path(tempdir(), "ShowCaseTempFolder")
      dir.create(dir_out, recursive = TRUE)
      
      # general data
      AllData <- data.frame(graph=c
                            (
                              "plot(ScatterData, main = 'Scatterplot')"
                              ,"plot(BarData, main = 'Histogram')"
                              ,"plot(BarData, rnorm(150), main = 'Boxplot')"
                              ,"plot(TimeSeriesData, main = 'Time-series')"
                              ,"plot(FunctionData, -10, 5*pi, main = 'Plot a function')"
                              ,"plot(IrisData, main = 'Correlation plot for two variables')"
                              ,"plot(IrisData, main = 'Correlation plot for two variables with lines of SS')
                                 lines(lowess(iris[,1:2]))"
                              ,"plot(IrisData3, main = 'Correlation plot for three or more')"
                              
                            ),
                            data=c('ScatterData <- cbind(rnorm(200),rnorm(200) * 
                                   rnorm(200) + rnorm(200))'
                                   ,'BarData <- factor(iris$Sepal.Width)'
                                   ,'BarData <- factor(iris$Sepal.Width)'
                                   ,'TimeSeriesData <- ts(matrix(rnorm(300),  
                                        nrow = 300, ncol = 1), start = c(1990, 1), 
                                        frequency = 12)'
                                   ,'FunctionData <- function(x) {x^4*pi}'
                                   ,'IrisData <- as.data.frame(iris[, 1:2])'
                                   ,'IrisData <- as.data.frame(iris[, 1:2])'
                                   ,'IrisData3 <- as.data.frame(iris[, 1:3])'
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
          text(x = 0.5, y = 0.5, paste("Graph code: ", graphinfo), cex = 1.5, col = "Darkgreen", font=1, adj=0.5)
          
          plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("Data script: ",datainfo), cex = 1.5, col = "Darkblue", font=1, adj=0.5)
          
          par(mfrow=c(1,1)) 
          dev.off()
        }
        
      
      # Render animation and store to disk
      plot_animation <- image_animate(image_join(lapply(list.files(dir_out, full.names = TRUE), image_read)), fps = 0.5)
      image_write(image = plot_animation,path = SavePath)
      unlink(dir_out, recursive=TRUE)

}


Store_path <- 'C:\\Users\\Tomaz\\Desktop\\ShowCase.gif'
plot_animation(Store_path)
