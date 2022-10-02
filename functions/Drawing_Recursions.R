
##################################
###
### Drawing recursion with ggplot2
###
##################################

library(ggplot2)

# function for circle

circle <- function(center=c(1,1),rr = 4, p = 100){
  r = rr / 2
  tt <- seq(0,2*pi,length.out = p)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


df = data.frame(x = xx, y = yy)
df2 = data.frame(x = xx, y = yy)
df$g <- "1"
df2$g <- "2"
dd <- rbind(df, df2)


ggplot(dd, aes(x,y)) + geom_path()
ggplot(circle(c(-1,1),rr=1),aes(x,y)) + geom_path() + theme_minimal()



drawCircle <- function(x,y,d,step){
  while (step >= 0){
    #drawCircle(x+20,y,d-0.1,step-1)
    x = x + 20
    y = y + 10
    d =d - 0.1
    step = step - 1
   # p <-  ggplot(circle(c(x,y),rr=d),aes(x,y)) + geom_path() + theme_minimal()
  #  p <- p + ggplot(circle(c(x,y),rr=d),aes(x,y)) + geom_path() + theme_minimal()
  }
  print(p)
}


drawCircle(-1,1,1,3)

