# 
# Life-Latz (Collatz) functions in R
# Useless-Usefull R functions
# 
# 


collatz_step <- function(n) if (n %% 2 == 0) n/2 else 3*n + 1

collatz_sequence <- function(n, limit = 1e6) {
  stopifnot(n > 1, n == as.integer(n))
  x <- n
  seq <- n
  for (i in 1:limit) {
    if (x == 1) break
    x <- collatz_step(x)
    seq <- c(seq, x)
  }
  seq
}

total_stopping_time <- function(n) {
  s <- collatz_sequence(n)
  length(s) 
}

max_height <- function(n) {
  max(collatz_sequence(n))
}


nice_axis <- function() {
  box(lwd=1.2)
  grid(col = "grey90")
}



plot_trajectories <- function(starts = c(7, 13, 27, 97)) {
  cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
  ymax <- 0
  seqs <- lapply(starts, collatz_sequence)
  ymax <- max(sapply(seqs, max))
  plot(NA, xlim = c(0, max(sapply(seqs, length))-1), ylim = c(1, ymax),
       xlab = "step", ylab = "value", main = "Hailstone trajectories (chores to couch)", log="y")
  nice_axis()
  for (i in seq_along(starts)) {
    lines(0:(length(seqs[[i]])-1), seqs[[i]], type="o", pch=16, cex=0.6, col = cols[i])
  }
  legend("topright", legend = paste("start =", starts), col = cols, lty = 1, pch = 16, bty="n")
  mtext("Odd = triple + 1 (drama), Even = half (progress). Eventually → 1 = couch.", cex=0.8)
}


plot_stopping_hist <- function(N = 5000) {
  times <- sapply(2:N, total_stopping_time)
  hist(times, breaks = 50, col = "#a6cee3", border = "white",
       main = sprintf("Time-to-couch across chores (2..%d)", N),
       xlab = "total stopping time (steps to reach 1)")
  nice_axis()
  abline(v = median(times), lwd=2, lty=2)
  mtext(sprintf("median = %d steps; some chores are drama queens.", median(times)), cex=0.8)
}


plot_max_height <- function(N = 2000) {
  starts <- 2:N
  heights <- sapply(starts, max_height)
  plot(starts, heights, pch = 20, cex = 0.5, col = "#33a02c",
       xlab = "starting chore (number)", ylab = "max chaos reached",
       main = "How chaotic before calm? (max height vs start)", log="y")
  nice_axis()
  mtext("Big, weird spikes happen… but we still end up on the couch.", cex=0.8)
}


plot_survival <- function(N = 5000) {
  times <- sapply(2:N, total_stopping_time)
  kmax <- max(times)
  counts <- tabulate(times, nbins = kmax)
  cum_done <- c(0, cumsum(counts))
  S <- ((N - 1) - cum_done) / (N - 1)
  plot(0:kmax, S, type = "s", lwd = 2,
       xlab = "steps k", ylab = "fraction still not at 1",
       main = "Empirical survival: chores still not on the couch after k steps")
  nice_axis()
  abline(h = 0, lwd = 1.2)
  mtext("The line keeps dropping. Empirical ‘proof by Sunday evening’.", cex = 0.8)
}


# zlepljenka
life_collatz_kolaz <- function() {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(2,2), mar = c(4,4,3,1))
  plot_trajectories()
  plot_stopping_hist()
  plot_max_height()
  plot_survival()
  invisible(NULL)
}

life_collatz_kolaz()




