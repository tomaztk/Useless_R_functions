getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")

##########################################
# 
# Function for Vanishing sentences
#
# Series:
# Little Useless-useful R functions #66
# Created: January 05, 2035
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


library(ggplot2)
library(gganimate)

# Function

vanishing_sentence <- function(sentence, output_file = NULL, interval = 0.5) {
  words <- unlist(strsplit(sentence, " "))
  
  vanishing_data <- do.call(rbind, lapply(seq_along(words), function(i) {
    remaining_words <- paste(words[1:(length(words) - i + 1)], collapse = " ")
    data.frame(step = i, text = remaining_words)
  }))
  

  vanishing_data <- rbind(
    vanishing_data,
    data.frame(step = max(vanishing_data$step) + 1, text = "")
  )

  p <- ggplot(vanishing_data, aes(x = 1, y = 1, label = text)) +
    geom_text(size = 6, hjust = 0.5, vjust = 0.5, fontface = "bold") +
    theme_void() +
    theme( plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(fill = "white", color = NA) ) +
    transition_states( step, transition_length = 1, state_length = 1 ) +
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
  
  

# Function usage
sentence <- "This sentence will gradually disappear word by word"
vanishing_sentence(sentence, output_file = "vanishing_sentence.gif")

vanishing_sentence(sentence)

 