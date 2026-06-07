##########################################
# 
# Useless useful R functions
# Ulam prime number spiral
#
# Series:
# Little Useless-useful R functions #93
# Created: June 03, 2026
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.1

###########################################



ulam_prime_spiral <- function(
    n         = 51,    
    theme     = c("Cosmic","Blueish","Classy","Psycho"),
    show_nums = FALSE,   # print values (n ≤ 21 only)
    animate   = FALSE,       
    speed     = 1,          
    verbose   = TRUE
) {
  
  theme <- match.arg(theme)
  
  #n must be odd so integers have a unique centre cell 
  if (n %% 2 == 0) { n <- n + 1L }
  if (n < 5) stop("Size muste be > 5.")
  total <- n^2
  mid   <- (n + 1L) / 2L            
  
  dr <- c( 0L, -1L,  0L,  1L)   # row deltas:  E  N  W  S
  dc <- c( 1L,  0L, -1L,  0L)   # col deltas:  E  N  W  S
  
  mat   <- matrix(0L, n, n)     
  ord_r <- integer(total)        
  ord_c <- integer(total)        
  
  r <- mid;  
  cc <- mid  
  
  mat[r, cc] <- 1L
  ord_r[1]   <- r
  ord_c[1]   <- cc
  
  d    <- 1L  # current direction index (1=E 2=N 3=W 4=S)
  step <- 1L  # current arm length
  num  <- 2L                     
  
  while (num <= total) {
    for (half in 1:2) {          # each dir is twice 
      for (i in seq_len(step)) {
        r  <- r  + dr[d]
        cc <- cc + dc[d]
        mat[r, cc] <- num
        ord_r[num] <- r
        ord_c[num] <- cc
        num <- num + 1L
        if (num > total) break  
      }
      d <- (d %% 4L) + 1L       # turn left: E→N→W→S→E
      if (num > total) break    
    }
    step <- step + 1L            
  }
  
  # SIEVE OF ERATOSTHENES
  #  Returns a logical vector: is_prime[k] == TRUE  iff  k is prime.
  
  is_prime    <- rep(TRUE, total)
  is_prime[1] <- FALSE
  p <- 2L
  while (p * p <= total) {
    if (is_prime[p])
      is_prime[seq.int(p * p, total, p)] <- FALSE
    p <- p + 1L
  }

  prime_mat <- matrix(is_prime[mat], n, n)
  n_primes  <- sum(prime_mat)
  density   <- 100 * n_primes / total
  
  # themes
  th <- switch(theme,
               Cosmic = list(
                 bg       = "#000000",
                 prime    = "#FFD700",     
                 comp     = "#0d0d1a",
                 txt      = "#FFD700"
               ),
               Blueish = list(
                 bg       = "#071626",
                 prime    = "#e0f0ff",     
                 comp     = "#112240",
                 txt      = "#7ecbf0"
               ),
               Classy = list(
                 bg       = "#f8f8f8",
                 prime    = "#111111",     
                 comp     = "#e4e4e4",
                 txt      = "#333333"
               ),
               Psycho = list(
                 bg       = "#080808",
                 prime    = NULL,          
                 comp     = "#1a1a1a",
                 txt      = "#ffffff"
               )
  )
  

  col_mat <- matrix(th$comp, n, n)
  if (theme == "Psycho") {
    ramp      <- colorRampPalette(
      c("#ff0055","#ff6600","#ffee00","#33ff99","#0099ff","#cc00ff"))
    prime_pos <- which(prime_mat) 
    vals      <- mat[prime_pos]                   
    t_norm    <- (vals - 2) / max(1L, total - 2L)  # normalise for better visuals
    pal       <- ramp(512)
    col_mat[prime_pos] <- pal[pmax(1L, ceiling(t_norm * 512))]
  } else {
    col_mat[prime_mat] <- th$prime
  }
  
  ## Plot
  display <- t(col_mat)[, n:1]  
  u_cols  <- unique(as.vector(display))
  idx_mat <- matrix(match(display, u_cols), n, n)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(bg  = th$bg,pty = "s",mar = c(2.2, 0.3, 2.2, 0.3))
  
  if (!animate) {
    image(seq_len(n), seq_len(n), idx_mat,
          col  = u_cols,
          axes = FALSE,
          xlab = "", ylab = "")
  } else {
    plot.new()
    plot.window(xlim = c(0.5, n + 0.5), ylim = c(0.5, n + 0.5))
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = th$bg, border = NA)
    
    delay <- max(0, 0.008 / speed)
    flush_every <- max(1L, as.integer(30L * speed))
    
    for (k in seq_len(total)) {
      r_k  <- ord_r[k]
      c_k  <- ord_c[k]
      y_k  <- n + 1L - r_k #flip the asix
      fill <- col_mat[r_k, c_k]
      
      rect(c_k - 0.5, y_k - 0.5, c_k + 0.5, y_k + 0.5, col = fill, border = NA)
      
      if (k %% flush_every == 0L) {
        if (existsMethod("dev.flush", "ANY"))
          try(dev.flush(), silent = TRUE)
        Sys.sleep(delay)
      }
    }
  }
  
  mtext(
    sprintf("Ulam Prime Spiral  \u00b7  %d\u00d7%d  \u00b7  %d primes  \u00b7  %.1f%%", n, n, n_primes, density),
    side = 3, line = 0.6, col = th$txt, cex = 0.85, font = 2
  )
  mtext(
    th$sub, side = 1, line = 0.8,
    col  = adjustcolor(th$txt, alpha.f = 0.55), cex = 0.62
  )
  
  #  Center and  image coords:  x = col,  y = n+1-row
  if (show_nums) {
    if (n > 21) {
      message("show_nums is only legible for n \u2264 21; skipped.")
    } else {
      num_cex <- max(0.28, 0.85 - (n - 5) * 0.035)
      for (r in seq_len(n)) {
        for (cc in seq_len(n)) {
          is_p <- prime_mat[r, cc]
          text(x      = cc,
               y      = n + 1L - r,
               labels = mat[r, cc],
               col    = if (is_p) th$bg else adjustcolor(th$txt, 0.30),
               cex    = num_cex,
               font   = if (is_p) 2L else 1L)
        }
      }
    }
  }
  
# results
  if (verbose) {
    cat("Stats: \n")
    cat(sprintf("  Grid              : %d \u00d7 %d = %d integers\n", n, n, total))
    cat(sprintf("  Primes found      : %d\n", n_primes))
    cat(sprintf("  Prime density     : %.2f%%\n", density))
    cat(sprintf("  Expected (1/ln n) : %.2f%%\n", 100 / log(total)))
    cat(sprintf("  Centre value      : %d\n", mat[mid, mid]))
    cat(sprintf("  Corner values     : %d  %d  %d  %d\n", mat[1,1], mat[1,n], mat[n,1], mat[n,n]))

    main_d <- diag(prime_mat)
    anti_d <- prime_mat[cbind(seq_len(n), rev(seq_len(n)))]
    cat(sprintf("  Main diagonal     : %d/%d primes (%.0f%%)\n", sum(main_d), n, 100 * mean(main_d)))
    cat(sprintf("  Anti-diagonal     : %d/%d primes (%.0f%%)\n", sum(anti_d), n, 100 * mean(anti_d)))
    
    # Mini ASCII visuals
    if (n <= 21) {
      cat(sprintf("\n  Mini ASCII spiral (n=%d) — [P]=prime  [.]=composite\n\n", n))
      for (r in seq_len(n)) {
        cat("  ")
        for (cc in seq_len(n)) {
          cat(if (prime_mat[r, cc]) sprintf("[%2d]", mat[r,cc]) else sprintf(" %2d ", mat[r,cc]))
        }
      }
    }
  }
}


################
# run the script
################

ulam_prime_spiral(n = 11, theme = "Classy", show_nums = TRUE)
ulam_prime_spiral(n = 201, theme = "Cosmic")
ulam_prime_spiral(n = 101, theme = "Blueish")
ulam_prime_spiral(n = 151, theme = "Psycho")
  
# with animiation
ulam_prime_spiral(n = 51, theme = "Cosmic", animate = TRUE, speed = 2)
  
