## Packages
## distro animation

library(ggplot2)
library(gganimate)
library(magick)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc" 
MyPurple5 <-  "#9c669c"

## Create Data

data<- data.frame("value"=c(runif(4000, 0, 20), 
                            rbinom(3000, 20, 0.5) + rnorm(3000, 0, 0.3),
                            runif(1000, 0, 20), 
                            rbinom(2000, 20, 0.7)+ rnorm(2000, 0, 0.3), 
                            runif(2000, 0, 20), 
                            rbinom(2000, 20, 0.1)+ rnorm(2000, 0, 0.3),
                            runif(2000, 0, 20)),
                  "time"=rep(1:4, each = 4000))   


## Create Animated Graphs

g1 <- ggplot(data, aes(x="",y = value)) +
  geom_boxplot(fill = MyPurple5, color = "black",lwd = 1.5, fatten = 1) + 
  coord_flip() +
  geom_jitter(width = 0.25, color = MyPurple, size = 2, alpha = 0.2) +
  transition_states(time, transition_length = 3, state_length = 2 ) +
  enter_fade() +
  exit_fade() +
  theme(plot.title=element_text(size=20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill=MyLightP), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
  ) +
  labs(title="Histogram and Boxplot", y = NULL, x = NULL)

g2 <- ggplot(data, aes(x = value)) +
  geom_histogram(bins = 30, fill = MyPurple5, color = "black") +
  transition_states(time, transition_length = 3, state_length = 2 ) +
  enter_fade() +
  exit_fade() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        plot.caption = element_text(hjust = c(1), size = c(14), 
                                    color = c(MyPurple) )
  ) +
  labs(title = NULL,	y = NULL,	x = NULL,
       caption=c("Thanks") )


BoxPlotAnimate <- animate(g1 ,  fps = 5, duration = 10,
                          width = 1456 / 2, height = (936 / 2) / 2,
                          renderer = magick_renderer() )

HistPlotAnimate <- animate(g2 , fps = 5, duration = 10,
                           width = 1456 / 2, height = 3 * (936 / 2) / 4,
                           renderer = magick_renderer() )

## Combine the two animated graph into one image

HistBoxAnimate <- image_append(c(BoxPlotAnimate[1],HistPlotAnimate[1]), stack = TRUE)

for( i in 2:50) {
  TempGif <- image_append(c(BoxPlotAnimate[i], HistPlotAnimate[i]), stack = TRUE)
  HistBoxAnimate <- c(HistBoxAnimate, TempGif)
}


## Save graph

anim_save("HistBoxAnimate.gif", HistBoxAnimate)

getwd()
