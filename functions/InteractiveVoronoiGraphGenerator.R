#######################
#
# Interactive Voronoi
# graph generator
#
#
#
#######################



# packages
library(deldir)
library(ggplot2)
dff <- NULL
n <- 0


#### Board Graph 
voronoiGraphBoard <- function(){
 r <<-  ggplot(data=dff, aes(x=long,y=lat)) +
    geom_segment( aes(x = x1, y = y1, xend = x2, yend = y2), size = 2, data = voronoi$dirsgs, linetype = 1, color= "#FFB958") + 
    geom_point( fill=rgb(70,130,180,255,maxColorValue=255), pch=21, size = 4,color="#333333") 
  
 r
}




click <- function(voronoiGraphBoard = defaultGraph){
    for (n in 1:10) {
      plot.new()
      mouse.at <- locator(n = 1, type = "o") 
      long <- rnorm(1, mouse.at$x*10,15)
      lat <- rnorm(1, mouse.at$y*10,10)
      df <- data.frame(lat,long)
      dff <- rbind(dff, df)
      
      if (nrow(dff)>=2){
        voronoi <- deldir(dff$long, dff$lat)
        voronoiGraphBoard()
        
      }
      n <- n + 1
    }  
      Sys.sleep(1)
      defaultGraph <<- voronoiGraphBoard
  }
  



### Start with x11 
Draw_x11 <- function(){
  x11()
  defaultGraph <<- voronoiGraphBoard() 
  click()
}

# Generate Voronoi
Draw_x11()


