
#####

inscrp <- function(nof){
          x <- rnorm(nof)
          y <- rnorm(nof)
          plot(x,y, pch = 1, col = "white",  xaxt='n',  yaxt='n', ann=FALSE, frame.plot=FALSE)
          xspline(x,y, 1, draw = TRUE, col="blue")
}

##### run

inscrp(10)
inscrp(50)
inscrp(100)
