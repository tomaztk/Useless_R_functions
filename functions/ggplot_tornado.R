### kind of tornado
library(ggplot2)
library(gganimate)

angle <- 2.0
points <- 1000

t <- (1:points)*angle
x <- cos(t)
y <- acosh(t)
x2 <- sin(t)

df <- data.frame(t, x, y,x2)

p <- ggplot(df, aes(x*t, y*t))
p <- p + 
  geom_point(aes(size=t),shape=3,alpha=0.5,color="brown") +
  theme(panel.background=element_rect(fill="lightblue"), 
        panel.grid=element_blank(),axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),legend.position="none")

p2 <- ggplot(df, aes(x2*t, y*t))
p2 <- p2 + 
  geom_point(aes(size=t),shape=3,alpha=0.5,color="brown") +
  theme(panel.background=element_rect(fill="lightblue"), 
        panel.grid=element_blank(),axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),axis.title.x=element_blank(),
        axis.title.y=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),legend.position="none")


animation = animate(c(p,p2), width = 400, height = 400)
animation
