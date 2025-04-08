##########################################
# 
# Entropy-meter
# 
#
# Series:
# Little Useless-useful R functions #64
# Created: April 06, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################



library(ggplot2)
library(dplyr)
library(stringr)



entropy_meter <- function(tweet) {
  tweet <- str_to_lower(tweet) %>% str_remove_all("\\s")
  chars <- unlist(strsplit(tweet, split = ""))
  
  freq_table <- table(chars)
  probs <- freq_table / sum(freq_table)
  
  # Shannon entropy
  entropy <- -sum(probs * log2(probs))
  
  list(
    tweet = tweet,
    entropy = entropy,
    char_probs = data.frame(char = names(probs), prob = as.numeric(probs))
  )
}


tweets <- c(
  "hello world!",
  "aaaaaaahhhhhhh",
  "covfefe",
  "orange man with number 47",
  "The quick brown fox jumps over the lazy dog.",
  "BUY $DOGE 🚀 TO THE MOON!!!"
)

results <- lapply(tweets, entropy_meter)

data.frame(
  tweet = tweets,
  entropy = sapply(results, function(x) x$entropy)
)


most_chaotic <- results[[which.max(sapply(results, function(x) x$entropy))]]

ggplot(most_chaotic$char_probs, aes(x = reorder(char, -prob), y = prob)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Character Distribution of a Tweet",
    subtitle = paste("Shannon Entropy:", round(most_chaotic$entropy, 3), "bits"),
    x = "Character",
    y = "Probability"
  ) +
  theme_minimal()


entropy_meter("The mitochondria is the powerhouse of the cell.")
entropy_meter("LOLOLOLOLOL")
entropy_meter("🤖✨🧠👾🌈🦄")  # Emojis count too!



#########
### ### ### ### ### ### ### ### 
# Or slighttly better - compare
### ### ### ### ### ### ### ### 


entropy_meter_compare <- function(tweets) {
  compute_entropy <- function(tweet) {
    tweet_clean <- tolower(gsub("\\s", "", tweet))
    chars <- unlist(strsplit(tweet_clean, split = ""))
    if (length(chars) == 0) return(0)
    freq_table <- table(chars)
    probs <- as.numeric(freq_table) / sum(freq_table)
    entropy <- -sum(probs * log2(probs))
    
    return(entropy)
  }

  entropy_vals <- sapply(tweets, compute_entropy)
  
  result <- data.frame(
    Tweet = tweets,
    `Entropy (bits)` = round(entropy_vals, 3),
    check.names = FALSE
  )
  
  return(result[order(-result$`Entropy (bits)`), ]) 
}


### Run function for comparison
entropy_meter_compare(tweets)

