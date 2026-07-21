library(ggplot2)
library(gganimate)


solve_jugs <- function(caps  = c(16, 11, 7),
                       start = c(16,  0, 0),
                       goal  = c( 8,  8, 0)) {
  key     <- function(s) paste(s, collapse = "-")
  queue   <- list(list(state = start, path = list(start)))
  visited <- list()
  
  while (length(queue) > 0) {
    node  <- queue[[1]];  queue <- queue[-1]
    s     <- node$state
    if (isTRUE(all(s == goal))) return(node$path)
    if (!is.null(visited[[key(s)]])) next
    visited[[key(s)]] <- TRUE
    n <- length(s)
    for (from in seq_len(n)) for (to in seq_len(n)) {
      if (from == to || s[from] == 0 || s[to] == caps[to]) next
      pour <- min(s[from], caps[to] - s[to])
      new_s <- s;  new_s[from] <- s[from] - pour;  new_s[to] <- s[to] + pour
      if (is.null(visited[[key(new_s)]])) {
        queue <- c(queue, list(list(state = new_s, path  = c(node$path, list(new_s)))))
      }
    }
  }
  NULL
}

solution <- solve_jugs()
n_steps  <- length(solution)

#df in tidy

caps       <- c(16, 11, 7)
jug_names  <- c("16 L", "11 L", "7 L")
jug_x      <- c(1, 2, 3)
jug_widths <- c(0.55, 0.55, 0.55)
MAX_CAP    <- max(caps)

step_action <- function(prev, curr) {
  d <- curr - prev
  if (all(d == 0)) return("Go!")
  sprintf("%s  \u2192  %s", jug_names[which(d < 0)], jug_names[which(d > 0)])
}

frames <- do.call(rbind, lapply(seq_along(solution), function(i) {
  s       <- solution[[i]]
  action  <- if (i == 1) "Start" else step_action(solution[[i-1]], s)
  is_goal <- all(s == c(8, 8, 0))
  do.call(rbind, lapply(seq_along(s), function(j) {
    data.frame(step = i, jug_label = jug_names[j], jug_x = jug_x[j],
               jug_width = jug_widths[j], capacity = caps[j], water = s[j],
               action = action, is_goal = is_goal, stringsAsFactors = FALSE)
  }))
}))

frames$shell_top   <- frames$capacity / MAX_CAP
frames$water_top   <- frames$water    / MAX_CAP
frames$water_label <- paste0(frames$water, " L")
frames$label_y     <- pmax(frames$water_top / 2, 0.03)

step_subtitles <- sapply(seq_along(solution), function(i) {
  action <- if (i == 1) "Start" else step_action(solution[[i-1]], solution[[i]])
})

# Static tick-mark data
tick_df <- do.call(rbind, lapply(seq_along(caps), function(j) {
  ticks <- seq(0, caps[j], by = 2)
  data.frame(jug_x = jug_x[j], jug_width = jug_widths[j],
             tick_y = ticks / MAX_CAP,
             tick_lab = ifelse(ticks %% 4 == 0, paste0(ticks, "L"), ""),
             stringsAsFactors = FALSE)
}))


BG_COL     <- "white"
SHELL_COL  <- "black"
WATER_BLUE <- "lightblue"
WATER_GREEN <- "lightgreen"

p <- ggplot(frames) +
  
  geom_rect(aes(xmin = jug_x - jug_width/2, xmax = jug_x + jug_width/2,
                ymin = 0, ymax = shell_top),
            fill = "white", color = SHELL_COL, linewidth = 0.8) +
  
  geom_rect(aes(xmin = jug_x - jug_width/2 + 0.013,
                xmax = jug_x + jug_width/2 - 0.013,
                ymin = 0, ymax = water_top, fill = is_goal),
            show.legend = FALSE) +
  scale_fill_manual(values = c("FALSE" = WATER_BLUE, "TRUE" = WATER_GREEN)) +
  
  geom_segment(data = subset(frames, water > 0),
               aes(x = jug_x - jug_width/2 + 0.013,
                   xend = jug_x + jug_width/2 - 0.013,
                   y = water_top, yend = water_top),
               color = "white", linewidth = 0.8, alpha = 0.75) +
  
  geom_text(aes(x = jug_x, y = label_y, label = water_label), 
            color = "white", fontface = "bold", size = 4.4) +
  geom_text(aes(x = jug_x, y = shell_top + 0.048, label = jug_label),
            color = SHELL_COL, size = 4.0) +
  geom_segment(data = tick_df,
               aes(x = jug_x + jug_width/2, xend = jug_x + jug_width/2 + 0.055,
                   y = tick_y, yend = tick_y),
               color = SHELL_COL, linewidth = 0.35, inherit.aes = FALSE) +
  geom_text(data = subset(tick_df, tick_lab != ""),
            aes(x = jug_x + jug_width/2 + 0.10, y = tick_y, label = tick_lab),
            color = SHELL_COL, size = 2.6, hjust = 0, inherit.aes = FALSE) +
  
  scale_x_continuous(limits = c(0.25, 3.95), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.11, 1.28), expand = c(0, 0)) +
  
  labs(title   = "Water Jug Puzzle  |  16 L / 11 L / 7 L  \u2192  8 L + 8 L") +
  theme_void(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = BG_COL, color = NA),
    panel.background = element_rect(fill = BG_COL, color = NA),
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5, color = "#2C2C2A", margin = margin(t=14, b=4)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#5F5E5A", margin = margin(b=20)),
    plot.caption  = element_text(size = 9,  hjust = 0.5, color = "#888780", margin = margin(t=8, b=12)),
    plot.margin   = margin(10, 28, 10, 28)
  ) +
  
  transition_states(step, transition_length = 0, state_length = 2, wrap = FALSE) +
  labs(subtitle = "{step_subtitles[as.integer(closest_state)]}") +
  ease_aes("cubic-in-out") + enter_fade() + exit_fade()



getwd()
#Animation
animate(p,
        nframes   = n_steps * 10,
        fps       = 10,
        width     = 480,
        height    = 320,
        renderer  = gifski_renderer("Water_jug_puzzle_solver.gif"),
        end_pause = 40)
