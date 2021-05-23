# Looping through variable names and plotting graph

library(ggplot2)


#get columns
variableR <- names(iris)[1:4]



Iris_plot <- function(df=iris, .y_var) {
    y_var <- sym(.y_var)
  ggplot(df, aes(x = Species, y = !! y_var)) + 
    geom_boxplot(notch = TRUE) +
    theme_classic(base_size = 12)
}


for(varR in variableR){
  # pdf(paste0(varR, ".pdf"))
  print(Iris_plot(df=iris,.y_var=varR))
  # dev.off()
}



