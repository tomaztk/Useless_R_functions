library(grDevices)

N     <- 264
COLS  <- 22
ROWS  <- 12
S1    <- 1922110153   # LCG seed for position jitter
S2    <- 1769133315   # LCG seed for angle jitter

CELL_W <- 10
CELL_H <- 10

random <- function(seed) {
  list(
    nextval = function(a, b) {
      seed <<- (seed * 5) %% 2147483648
      seed / 2147483648 * (b - a) + a
    }
  )
}


norm01 <- function(x) (x - min(x)) / (max(x) - min(x))

generate_series <- function(n) {
  t    <- seq(0, 1, length.out = n)
  base <- 2 * pi * 8 * t
  
  raw_a <- sin(base) + 0.45 * sin(base * 2.1) + 0.18 * sin(base * 4.7)
  ph    <- pi * (0.8 * sin(pi * t * 3) + 0.35 * sin(pi * t * 0.8))
  raw_b <- sin(base + ph) + 0.45 * sin((base + ph) * 2.1) + 0.18 * sin((base + ph) * 4.7)
  
  data.frame(
    step        = seq_len(n),
    col         = ((seq_len(n) - 1) %% COLS) + 1,
    row         = ((seq_len(n) - 1) %/% COLS) + 1,
    series_a    = norm01(raw_a),
    series_b    = norm01(raw_b)
  )
}

df             <- generate_series(N)
df$abs_diff    <- abs(df$series_a - df$series_b)
df$fragmentation <- df$abs_diff / max(df$abs_diff)   # normalised to [0, 1]

draw_square_frag <- function(gx, gy, frag, r1, r2, col_fill) {
  move_limit  <- min(CELL_W, CELL_H) * 0.38 * frag
  twist_limit <- pi / 4 * frag
  r           <- sqrt(2) * min(CELL_W, CELL_H) / 2 * 0.86
  
  cx    <- gx + CELL_W / 2 + r1$nextval(-move_limit, move_limit)
  cy    <- gy + CELL_H / 2 + r1$nextval(-move_limit, move_limit)
  angle <- r2$nextval(pi / 4 - twist_limit, pi / 4 + twist_limit)
  
  x0 <- cx + r * sin(angle)
  y0 <- cy + r * cos(angle)
  
  for (step in 1:4) {
    angle <- angle + pi / 2
    x1    <- cx + r * sin(angle)
    y1    <- cy + r * cos(angle)
    segments(x0, y0, x1, y1, lwd = 1.75, col = col_fill)
    x0 <- x1
    y0 <- y1
  }
}

plot_w <- COLS * CELL_W
plot_h <- ROWS * CELL_H
ts_h   <- plot_h * 0.35  

getwd()
layout(matrix(c(1, 2), nrow = 2), heights = c(ts_h, plot_h))

par(mar = c(2, 3, 2, 1))
plot(
  NULL, NULL,
  xlim = c(1, N), ylim = c(-0.05, 1.05),
  axes = FALSE, ann = FALSE,
  xlab = "", ylab = ""
)

# Shaded |A − B| band
polygon(
  c(df$step, rev(df$step)),
  c(df$abs_diff, rep(0, N)),
  col = adjustcolor("grey60", alpha.f = 0.18),
  border = NA
)
lines(df$step, df$abs_diff, col = adjustcolor("grey40", alpha.f = 0.5), lwd = 1)
lines(df$step, df$series_a, col = "#378ADD", lwd = 1.6)
lines(df$step, df$series_b, col = "#D85A30", lwd = 1.6, lty = 2)

axis(1, at = c(1, 66, 132, 198, 264), labels = c(1, 66, 132, 198, 264),
     cex.axis = 0.7, col = "grey70", col.axis = "grey40")
axis(2, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"),
     cex.axis = 0.7, col = "grey70", col.axis = "grey40", las = 1)
mtext("Time step", side = 1, line = 1.2, cex = 0.7, col = "grey40")
mtext("Value", side = 2, line = 2, cex = 0.7, col = "grey40")

legend(
  "topright",
  legend = c("Series A", "Series B", "|A − B|"),
  col    = c("#378ADD", "#D85A30", adjustcolor("grey40", 0.6)),
  lty    = c(1, 2, 1),
  lwd    = c(1.6, 1.6, 1),
  bty    = "n",
  cex    = 0.7
)


par(mar = c(1, 1, 1, 1))
plot(
  NULL, NULL,
  xlim = c(0, plot_w), ylim = c(plot_h, 0),   # y flipped: row 1 at top
  axes = FALSE, ann = FALSE
)

r1 <- random(S1)
r2 <- random(S2)

for (k in seq_len(N)) {
  col_idx <- df$col[k]
  row_idx <- df$row[k]
  gx      <- (col_idx - 1) * CELL_W
  gy      <- (row_idx - 1) * CELL_H
  draw_square_frag(gx, gy, df$fragmentation[k], r1, r2, col_fill = "black")
}

