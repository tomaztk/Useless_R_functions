getwd()
setwd("/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions")


##########################################
# 
# Generating cognitive bias for all the
# hard-core psychologists :) and
# visualize the graph network
#
# Series:
# Little Useless-useful R functions #71
# Created: May 12, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com

# ToDo:
# get more sense to the useless function
# more samples from the psychology

###########################################


library(igraph)
library(ggraph)
library(ggplot2)
library(tibble)

bias_explorer <- function(seed = 2908, n_links = 25) {
  set.seed(seed)
  # Define cognitive biases
  biases <- c(
    "Confirmation Bias", "Anchoring Bias", "Availability Heuristic", 
    "Dunning-Kruger Effect", "Survivorship Bias", "Recency Bias",
    "Sunk Cost Fallacy", "Bandwagon Effect", "Framing Effect", 
    "Self-Serving Bias", "Negativity Bias", "Halo Effect"
  )
  
  # Generate weird links
  weird_links <- c(
    "You saw it on Reddit", "Too lazy to verify", "Sounds familiar",
    "Because Elon tweeted it", "Grandma said so", "Wikipedia said maybe",
    "Your gut feeling", "Cited by no one", "Used in a TED talk",
    "Found in fortune cookie", "Might be science", "Feels statistically valid"
  )
  
  # Randomly link biases with graph edge labels
  edges <- tibble(
    from = sample(biases, 20, replace = TRUE),
    to   = sample(biases, 20, replace = TRUE),
    reason = sample(weird_links, 20, replace = TRUE)
  ) %>% filter(from != to)
  
  #g <- graph_from_data_frame(edges, vertices = biases)
  g <- graph_from_data_frame(edges, vertices = data.frame(name = biases), directed = TRUE)
  
  
  # ggraph(g, layout = "fr") +
  #   geom_edge_link(aes(label = reason),
  #                  arrow = arrow(length = unit(3, 'mm')),
  #                  end_cap = circle(3, 'mm'),
  #                  start_cap = circle(3, 'mm'),
  #                  label_colour = "gray40", edge_width = 0.8,
  #                  colour = "skyblue") +
  #   geom_node_point(color = "darkred", size = 6) +
  #   geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 4) +
  #   labs(
  #     title = "Bias_explorer(): Cognitive Bias Web",
  #     subtitle = "Absurd connections between mental mistakes.",
  #     caption = "Edges represent whimsical or irrational connections."
  #   ) +
  #   theme_void()
  
  ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(label = reason),
      arrow = arrow(length = unit(4, 'mm')),
      end_cap = circle(3, 'mm'),
      start_cap = circle(3, 'mm'),
      label_colour = "gray40",
      edge_width = 0.8,
      colour = "skyblue"
    ) +
    geom_node_point(color = "darkred", size = 6) +
    geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 4) +
    labs(
      title = "Bias_explorer(): The Absurd Web of Biases",
      subtitle = "Visualizing ridiculous mental shortcuts.",
      caption = "Edges represent irrational (and hilarious) connections."
    ) +
    theme_void()
  
}


# Run the function
bias_explorer()
