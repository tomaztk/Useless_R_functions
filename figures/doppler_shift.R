##########################################
# 
# Doppler shift?
# 
#
# Series:
# Little Useless-useful R functions #75
# Created: OCtober  01, 2025
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(ggplot2)
library(reshape2)

doppler_shift <- function(frequencies, source_speeds, observer_speed = 0, c = 343) {
  shifted_freqs <- outer(frequencies, source_speeds, 
                         function(f, v_s) f * (c + observer_speed) / (c - v_s))
  
  return(shifted_freqs)
}

doppler_colormap <- function(frequencies = seq(100, 1000, by = 50), 
                             source_speeds = seq(-100, 100, by = 10), 
                             observer_speed = 0) {
  
 
  shifted_freqs <- doppler_shift(frequencies, source_speeds, observer_speed)
  
  # Convert to a data frame for ggplot
  df <- melt(shifted_freqs)
  colnames(df) <- c("Frequency_Index", "Speed_Index", "Shifted_Frequency")
  
  df$Frequency <- frequencies[df$Frequency_Index]
  df$Source_Speed <- source_speeds[df$Speed_Index]
  

  p <- ggplot(df, aes(x = Source_Speed, y = Frequency, fill = Shifted_Frequency)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", name = "Observed Frequency (Hz)") +
    theme_minimal() +
    labs(
      title = "Doppler-Shifted Sound Frequency Heatmap",
      subtitle = "Color intensity represents the frequency shift due to relative motion",
      x = "Source Speed (m/s)",
      y = "Original Frequency (Hz)"
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  print(p)
}


doppler_colormap()




