#####################
## Catalan number ###
#####################

library(ggplot2)
library(tidyverse)

# Function for factorial
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Function for n-th Catalan number
catalan <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(factorial(2 * n) / (factorial(n + 1) * factorial(n)))
  }
}



for (i in 0:10) {
  cat(sprintf("fun(%d) = %d\n", i, factorial(i)))
}


# Draw graph
df <- as.data.frame(NULL)
for (i in 0:10) {
  df_i <- print(i)
  cat_i <- catalan(i)
  fac_i <- factorial(i)
  df <- rbind(df, c(df_i, cat_i, fac_i))
}

colnames(df) <- c("i", "catalan", "factorial")

df$i <- as.integer(df$i)
df$catalan <- as.integer(df$catalan)
df$factorial <- as.integer(df$factorial)


ggplot(df, aes(x=i)) + 
  geom_line(aes(y = catalan), color = "darkgreen") + 
  geom_line(aes(y = factorial), color="steelblue") 

# or
df %>%
  select(i, catalan, factorial) %>%
  gather(key = "variable", value = "value", -i) %>%
  ggplot(aes(x = i, y = value)) + 
    geom_line(aes(color = variable, linetype = variable)) + 
    scale_color_manual(values = c("darkgreen", "steelblue"))

