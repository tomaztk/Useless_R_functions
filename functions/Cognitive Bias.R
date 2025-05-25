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

bias_explorer <- function(seed = 2908, n_links = 25) {
  set.seed(seed)
  
  # Some psych effects from RL
  biases <- c(
    "Confirmation Bias", "Anchoring Bias", "Availability Heuristic", 
    "Dunning-Kruger Effect", "Survivorship Bias", "Recency Bias",
    "Sunk Cost Fallacy", "Bandwagon Effect", "Framing Effect", 
    "Self-Serving Bias", "Negativity Bias", "Halo Effect"
  )
  
  # useless links :)
  weird_links <- c(
    "You saw it on Reddit", "Too lazy to verify", "Sounds familiar",
    "Because Elon tweeted it", "Grandma said so", "Wikipedia said maybe",
    "Your gut feeling", "Cited by no one", "Used in a TED talk",
    "Found in fortune cookie", "Might be science", "Feels statistically valid"
  )

  edges <- data.frame(
    from = sample(biases, n_links, replace = TRUE),
    to = sample(biases, n_links, replace = TRUE),
    reason = sample(weird_links, n_links, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  edges <- edges[edges$from != edges$to, ]
  g <- graph_from_data_frame(edges, vertices = data.frame(name = biases), directed = TRUE)

  ggraph(g, layout = "drl") +
    geom_edge_link(
      aes(label = reason),
      arrow = arrow(length = unit(3, 'mm')),
      end_cap = circle(2, 'mm'),
      start_cap = circle(2, 'mm'),
      label_colour = "darkgray",
      edge_width = 1.2,
      colour = "skyblue"
    ) +
    geom_node_point(color = "darkred", size = 6) +
    geom_node_text(aes(label = name), repel = TRUE, fontface = "bold", size = 3.5) +
    labs(
      title = "Bias_explorer(): The Absurd Web of Biases",
      subtitle = "Visualizing ridiculous mental shortcuts.",
      caption = "Edges represent irrational and useless connections."
    ) +
    theme_void()
  
}

##################
# Run the function
##################

bias_explorer()
