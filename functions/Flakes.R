
library(ggplot2)


print_flakes <- function(nof){
    df_snowflakes <- data.frame(
      x = runif(nof, 0, 20),
      y = runif(nof, 0, 20) )
    
    shps <- c("+", "*", "o")
    random_shape <- sample(shps, size = 1)
    
    snowflake_plot <- ggplot(df_snowflakes, aes(x, y)) +
      geom_point(shape = random_shape, size = 2.6, color = "lightblue") + theme_void()  
    
    print(snowflake_plot)
}


# run and print 1000 snowflakes
print_flakes(1000)

