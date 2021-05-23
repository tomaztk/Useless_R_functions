##########################################
# 
# Looping through variable names and
# plotting boxplots
#
# Series:
# Little Useless-useful R functions #22
# Created: May 22, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

library(ggplot2)


#get column names from sample dataset Iris
variableR <- names(iris)[1:4]


# Helper function
Iris_plot <- function(df=iris, y) {
  ggplot(df, aes(x = Species, y = !! sym(y) )) + 
    geom_boxplot(notch = TRUE) +
    theme_classic(base_size = 10)
}


# Main loop through the columns and dataset
for(varR in variableR){
  name <- paste0(varR, "_x_Species")
  png(paste0(name, ".png"))
  print(Iris_plot(df=iris, y=varR))
  dev.off()
}