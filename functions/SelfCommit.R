####
###
### Code that self commits
###
####
library(ggplot2)


PlotIris <- function() {

ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species))

system("sudo cd /Users/tomazkastrun/Documents/GitHub/Useless_R_functions/functions && git add SelfCommit.R && git commit -m 'update' && git push", intern = TRUE)
#system("git add SelfCommit.R", intern = TRUE)
#system("git commit -m 'update' ")
#system("git push")

}

PlotIris()
