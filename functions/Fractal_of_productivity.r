
#
# Fractal of Productivity  
#

draw_fractal_of_productivity <- function(
    tasks,
    depth = 6,
    seed = NULL,
    mode = c("recursive", "lsystem"),

    branch_factor = c(2, 3),
    base_angle = 28,
    angle_jitter = 10,
    shrink = 0.72,
    fatigue = 0.82,
    procrast_bias = 0.08,
    procrast_gain = 0.55,

    lsystem_axiom = "F",
    lsystem_rules = list(F = "FF-[-F+F+F]+[+F-F-F]"),
    lsystem_angle = 22.5,
    lsystem_len = 0.08,
  
    gravity = 0.02,                    # deadline gravity (downward pull per level)
    priority = NULL,                   # numeric priorities (parallel to tasks)
    minutes = NULL,                    # numeric "pomodoro minutes" (parallel to tasks)
    theme = c("dark", "paper"),        # theme toggle
    
    bg = NULL, trunk_col = NULL, leaf_col = NULL,
    procrast_col = NULL, title_col = NULL
) {

  `%||%` <- function(a, b) if (is.null(a)) b else a #LOL..... trolam mal klele tidyverse :D
  
  mode  <- match.arg(mode)
  theme <- match.arg(theme)
  
  if (!is.null(seed)) set.seed(seed)
  tasks <- as.character(tasks)
  if (length(tasks) == 0) tasks <- "Task"
  

  if (theme == "dark") {
    bg_d        <- "#0b1021"
    trunk_d     <- "#8bd3dd"
    leaf_d      <- "#ffc857"
    procrast_d  <- "#94a3b8"
    title_d     <- "#e2e8f0"
  } else { # paper
    bg_d        <- "#ffffff"
    trunk_d     <- "#374151"
    leaf_d      <- "#1f2937"
    procrast_d  <- "#9ca3af"
    title_d     <- "#111827"
  }
  
  bg           <- bg           %||% bg_d
  trunk_col    <- trunk_col    %||% trunk_d
  leaf_col     <- leaf_col     %||% leaf_d
  procrast_col <- procrast_col %||% procrast_d
  title_col    <- title_col    %||% title_d
  
  to_rad <- function(deg) deg * pi / 180
  fade <- function(hex, alpha) {
    rgb(col2rgb(hex)[1]/255, col2rgb(hex)[2]/255, col2rgb(hex)[3]/255,
        alpha = max(0, min(1, alpha)))
  }

  cycler <- function(v) {
    v <- if (is.null(v)) NA else v
    i <- 0L
    function() { i <<- i + 1L; v[(i-1L) %% length(v) + 1L] }
  }
  next_task    <- cycler(tasks)
  next_prio    <- cycler(priority)
  next_minutes <- cycler(minutes)
  
  # normalize priorities to [0.6, 1.25] multiplier
  prio_scale <- function(p) {
    if (is.na(p)) return(1)
    p <- as.numeric(p)
    if (!is.finite(p)) return(1)
    rng <- range(priority, na.rm = TRUE)
    if (diff(rng) == 0) return(1)
    scaled <- (p - rng[1]) / diff(rng)
    0.6 + scaled * (1.25 - 0.6)
  }
  
  p_procrast <- function(level) {
    plogis(procrast_bias + procrast_gain * (level / max(1, depth)))
  }
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(bg = bg, mar = c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim = c(-1.25, 1.25), ylim = c(0, 2.1))
  rect(-2, -2, 2, 3, col = bg, border = NA)
  
  # Title
  text(0, 2.03, "Fractal of Productivity", col = fade(title_col, 0.95),
       cex = 1.1, xpd = NA)
  text(0, 1.94, "\u2014 tasks branch faster than free time \u2014",
       col = fade(title_col, 0.55), cex = 0.7, xpd = NA)
  
# Recursive painter :)
  
  if (mode == "recursive") {
    draw_branch <- function(x, y, angle_deg, len, level, lw) {
      if (level > depth || len < 0.01) return()
      
      x2 <- x + len * sin(to_rad(angle_deg))
      y2 <- y + len * cos(to_rad(angle_deg)) - gravity * (level - 1)
      give_up <- runif(1) < p_procrast(level)
      col_seg <- if (give_up) fade(procrast_col, 0.35) else fade(trunk_col, 0.85)
      segments(x, y, x2, y2, lwd = lw, col = col_seg)
      
      if (give_up || level == depth) {
        lab <- next_task()
        pr  <- prio_scale(next_prio())
        mins <- next_minutes()
        text(
          x2, y2, labels = paste0("\u2713 ", lab),
          cex = 0.62 * (1.05 - level/(depth + 0.7)) * pr,
          col = if (give_up) fade(leaf_col, 0.35) else fade(leaf_col, 0.95),
          pos = sample(1:4, 1), xpd = NA
        )
    
        if (is.finite(as.numeric(mins))) {
          cex_pt <- 0.25 + 0.015 * as.numeric(mins)
          points(x2, y2, pch = 16, cex = cex_pt, col = fade(leaf_col, 0.45))
          points(x2, y2, pch = 1, cex = cex_pt, col = fade(leaf_col, 0.75), lwd = 0.8)
        }
        return()
      }
      
    
      k <- sample(branch_factor[1]:branch_factor[length(branch_factor)], 1)
      spread <- base_angle * (1 + 0.15 * runif(1, -1, 1))
      if (k == 1) spread <- 0
      offsets <- seq(-spread, spread, length.out = k)
      child_len <- len * shrink * runif(1, 0.92, 1.08)
      child_lw  <- max(0.2, lw * fatigue)
      
      for (off in offsets) {
        ang <- angle_deg + off + runif(1, -angle_jitter, angle_jitter)
        draw_branch(x2, y2, ang, child_len, level + 1, child_lw)
      }
    }
    
  
    draw_branch(0, 0.12, 0, 0.52, level = 1, lw = 5)
  }
  
  # --- L-system mode
  if (mode == "lsystem") {
    cur <- lsystem_axiom
    for (i in seq_len(depth)) {
      chars <- strsplit(cur, "", fixed = TRUE)[[1]]
      out <- character(length(chars))
      for (j in seq_along(chars)) {
        ch <- chars[j]
        if (!is.null(lsystem_rules[[ch]])) out[j] <- lsystem_rules[[ch]] else out[j] <- ch
      }
      cur <- paste0(out, collapse = "")
    }
    
   
    angle <- 0
    len0  <- lsystem_len
    x <- 0; y <- 0.12
    stack <- list()
    level <- 1L
    leaves <- list()
    
    drawF <- function() {
      len <- len0 * shrink^(level - 1)
      x2  <- x + len * sin(to_rad(angle))
      y2  <- y + len * cos(to_rad(angle)) - gravity * (level - 1)
      give_up <- runif(1) < p_procrast(level)
      col_seg <- if (give_up) fade(procrast_col, 0.35) else fade(trunk_col, 0.85)
      segments(x, y, x2, y2, lwd = max(0.2, 5 * fatigue^(level - 1)), col = col_seg)
      list(x = x2, y = y2, give_up = give_up, lvl = level)
    }
    
    chars <- strsplit(cur, "", fixed = TRUE)[[1]]
    n <- length(chars)
    for (i in seq_len(n)) {
      ch <- chars[i]
      if (ch == "F") {
        leaf <- drawF()
        next_is_F <- (i < n && chars[i + 1] == "F")
        if (leaf$give_up || !next_is_F) {
          leaves[[length(leaves) + 1L]] <- leaf
        }
        x <- leaf$x; y <- leaf$y
      } else if (ch == "+") {
        angle <- angle + lsystem_angle
      } else if (ch == "-") {
        angle <- angle - lsystem_angle
      } else if (ch == "[") {
        stack[[length(stack) + 1L]] <- list(x = x, y = y, angle = angle, level = level)
        level <- level + 1L
      } else if (ch == "]") {
        if (length(stack)) {
          st <- stack[[length(stack)]]
          stack[[length(stack)]] <- NULL
          x <- st$x; y <- st$y; angle <- st$angle
          level <- max(1L, st$level)
        }
      }
    }
    for (leaf in leaves) {
      lab <- next_task()
      pr  <- prio_scale(next_prio())
      mins <- next_minutes()
      text(
        leaf$x, leaf$y, labels = paste0("\u2713 ", lab),
        cex = 0.62 * (1.0 - min(0.6, leaf$lvl/(depth + 0.7))) * pr,
        col = if (leaf$give_up) fade(leaf_col, 0.35) else fade(leaf_col, 0.95),
        pos = sample(1:4, 1), xpd = NA
      )
      if (is.finite(as.numeric(mins))) {
        cex_pt <- 0.25 + 0.015 * as.numeric(mins)
        points(leaf$x, leaf$y, pch = 16, cex = cex_pt, col = fade(leaf_col, 0.45))
        points(leaf$x, leaf$y, pch = 1,  cex = cex_pt, col = fade(leaf_col, 0.75), lwd = 0.8)
      }
    }
  }
  
  invisible(NULL)
}



tasks <- c("Write intro","Collect data","Clean data","Make plots",
           "Draft post","Edit","Publish","Share")

set.seed(42)
draw_fractal_of_productivity(
  tasks = tasks,
  depth = 7,
  mode = "recursive",
  gravity = 0.025,
  priority = c(3,1,2,2,5,4,3,2),
  minutes  = c(25,15,40,10,30,20,15,50),
  theme = "dark"
)






# 1) Recursive mode + all variations
tasks <- c("Write intro","Collect data","Clean data","Make plots",
           "Draft post","Edit","Publish","Share")

set.seed(42)
draw_fractal_of_productivity(
  tasks = tasks,
  depth = 7,
  mode = "recursive",
  gravity = 0.025,            # stronger deadline droop
  priority = c(3,1,2,2,5,4,3,2),     # bigger label = higher priority
  minutes  = c(25,15,40,10,30,20,15,50),  # pomodoro petals
  theme = "dark"
)

# 2) Paper theme, gentler gravity
set.seed(7)
draw_fractal_of_productivity(
  tasks = tasks,
  depth = 6,
  mode = "recursive",
  gravity = 0.015,
  priority = 1:8,
  minutes  = seq(10, 45, length.out = 8),
  theme = "paper"
)

# 3) L-system mode (plant-like) with labels at leaves
set.seed(123)
draw_fractal_of_productivity(
  tasks = tasks,
  depth = 5,                   # L-system iterations
  mode = "lsystem",
  lsystem_axiom = "F",
  lsystem_rules = list(F = "FF-[-F+F+F]+[+F-F-F]"),
  lsystem_angle = 22.5,
  lsystem_len   = 0.09,
  gravity = 0.02,
  priority = c(5,4,3,2,1,2,3,4),
  minutes  = c(15,20,25,30,35,20,10,40),
  theme = "dark"
)
