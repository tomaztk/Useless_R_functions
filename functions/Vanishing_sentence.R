
##########################################
# 
# Function for Vanishing sentences
#
# Series:
# Little Useless-useful R functions #66
# Created: January 05, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


library(ggplot2)
library(gganimate)
library(tidyr)
library(dplyr)


vanishing_sentence <- function(sentence, output_file = NULL, interval = 0.5) {

  words <- unlist(strsplit(sentence, " "))
  vanishing_order <- sample(seq_along(words))
  
  sentence_data <- data.frame(
    word = words,
    position = seq_along(words),
    vanish_step = match(seq_along(words), vanishing_order)
  )
  
  # sequence
  animation_data <- do.call(rbind, lapply(1:(max(sentence_data$vanish_step) + 1), function(step) {
    sentence_data %>%
      mutate(visible = ifelse(vanish_step >= step, TRUE, FALSE)) %>%
      group_by(position) %>%
      summarize(word = ifelse(visible, word, ""), .groups = "drop") %>%
      mutate(step = step)
  }))

  p <- ggplot(animation_data, aes(x = position, y = 1, label = word)) +
    geom_text(size = 6, hjust = 0.5, vjust = 0.5, fontface = "bold") +
    theme_void() +
    theme(
      plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    transition_states(step, transition_length = interval,state_length = 1) +
    enter_fade() +
    exit_fade() +
    ease_aes("linear") +
    labs(title = "Vanishing Sentence Animation")
  
  
  # render and save
  if (!is.null(output_file)) {
    anim <- animate( p,nframes = length(words) + 10, fps = 10,width = 800,height = 400, renderer = gifski_renderer(output_file) )

    message("Animation saved to ", output_file)
    return(anim)
  } else {
    # or view if file is not specified
    animate(
      p,
      nframes = length(words) + 10,
      fps = 10,
      width = 800,
      height = 400
    )
  }
}
  
  

# Example usage


sentence <- "This sentence will gradually vanish - word by word"
# save to file
vanishing_sentence(sentence, output_file = "vanishing_sentence.gif")
# save to output
vanishing_sentence(sentence)


 