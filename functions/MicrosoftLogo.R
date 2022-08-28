
#################
# microsoft logo
#################
library(ggplot2)

#### Logo
ggplot()+ 
geom_rect(aes(xmin=1,xmax=3,ymin=10,ymax=15),fill="#05a6f0")+  
geom_rect(aes(xmin=1,xmax=3,ymin=15,ymax=20),fill="#f35426")+  
geom_rect(aes(xmin=3,xmax=5,ymin=10,ymax=15),fill="#ffba08")+  
geom_rect(aes(xmin=3,xmax=5,ymin=15,ymax=20),fill="#81bc06")+
theme_void()


