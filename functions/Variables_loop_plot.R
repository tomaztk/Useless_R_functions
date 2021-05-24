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
x <- names(iris)[5]

# Helper function
Iris_plot <- function(df=iris, x, y) {
  ggplot(df, aes(x = !! sym(x), y = !! sym(y) )) + 
    geom_boxplot(notch = TRUE) +
    ggtitle(paste0("Plot of ", y, " with ", x )) +
    theme_classic(base_size = 10)
}


# Main loop through the columns and dataset
for(varR in variableR){
  name <- paste0(varR, "_x_", x)
  png(paste0(name, ".png"))
  print(Iris_plot(df=iris, x=x, y=varR))
  dev.off()
}
