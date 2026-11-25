############################################################ 
# Useless useful R functions: Toeplitz' inscribed square problem
# #########################################################

library(ggplot2)

curve_blob <- function(seed = 2911, k = 5, wobble = 0.35) {
  set.seed(seed)
  a <- rnorm(k) * wobble / (1:k)
  b <- rnorm(k) * wobble / (1:k)
  function(t) {
    
    th <- 2 * pi * t
    
    r <- 1 +
      colSums(
        a * cos(outer(1:k, th)) +
          b * sin(outer(1:k, th))
      )
    
    cbind(
      x = r * cos(th),
      y = r * sin(th)
    )
  }
}

# v2
curve_blob <- function(seed = 2911, k = 5, wobble = 0.35) {
  set.seed(seed)
  a <- rnorm(k) * wobble / (1:k)
  b <- rnorm(k) * wobble / (1:k)
  function(t) {
    th <- 2 * pi * t
    r  <- 1 + colSums(a * cos(outer(1:k, th)) + b * sin(outer(1:k, th)))
    cbind(x = r * cos(th), y = r * sin(th))
  }
}

curve_from_points <- function(x, y) {
  n  <- length(x)
  tt <- seq(0, 1, length.out = n + 1)
  sx <- splinefun(tt, c(x, x[1]), method = "periodic")
  sy <- splinefun(tt, c(y, y[1]), method = "periodic")
  function(t) cbind(x = sx(t %% 1), y = sy(t %% 1))
}


make_square <- function(A, C) {
  M <- (A + C) / 2
  h <- C - M
  # Rotate diagonal half-vector by 90 degrees
  r <- cbind(-h[, 2], h[, 1])
  
  list(
    A = A,
    B = M + r,
    C = C,
    D = M - r
  )
}

signed_distance <- function(qx, qy, P) {
  
  n <- nrow(P)
  x1 <- P[, 1]
  y1 <- P[, 2]
  
  x2 <- c(P[-1, 1], P[1, 1])
  y2 <- c(P[-1, 2], P[1, 2])
  
  distance <- rep(Inf, length(qx))
  inside <- rep(FALSE, length(qx))
  
  for (i in seq_len(n)) {
    
    dx <- x2[i] - x1[i]
    dy <- y2[i] - y1[i]
    
    L <- max(dx^2 + dy^2, 1e-24)
    
    u <- ((qx - x1[i]) * dx +
            (qy - y1[i]) * dy) / L
    
    u <- pmin(1, pmax(0, u))
    
    d <- (qx - x1[i] - u * dx)^2 +
      (qy - y1[i] - u * dy)^2
    
    distance <- pmin(distance, d)
    
    # Ray-casting test
    crossing <-
      ((y1[i] > qy) != (y2[i] > qy)) &
      (qx < dx * (qy - y1[i]) /
         (y2[i] - y1[i]) + x1[i])
    
    inside <- xor(inside, crossing)
  }
  
  ifelse(inside, -1, 1) * sqrt(distance)
}


find_squares <- function(f, n = 150) {
  
  t <- (0:(n - 1)) / n
  P <- f(t)
  pairs <- expand.grid(i = 1:n, j = 1:n)
  
  A <- P[pairs$i, , drop = FALSE]
  C <- P[pairs$j, , drop = FALSE]
  
  squares <- make_square(A, C)
  dB <- signed_distance(
    squares$B[, 1],
    squares$B[, 2],
    P
  )
  
  dD <- signed_distance(
    squares$D[, 1],
    squares$D[, 2],
    P
  )
  
  # oba vogala morata biti na krivulji
  good <- abs(dB) < 0.03 &
    abs(dD) < 0.03
  
  good <- which(good)
  
  if (!length(good))
    return(NULL)
  
  # pripravi reultate 
  result <- lapply(good, function(i) {
    
    s <- list(
      A = A[i, ],
      B = squares$B[i, ],
      C = C[i, ],
      D = squares$D[i, ]
    )
    
    data.frame(
      x = c(s$A[1], s$B[1], s$C[1], s$D[1]),
      y = c(s$A[2], s$B[2], s$C[2], s$D[2])
    )
  })
  
  result
}


draw_solution <- function(f, squares) {
  curve <- as.data.frame(
    f(seq(0, 1, length.out = 1000))
  )
  
  p <- ggplot() +
    
    # Original curve
    geom_path(
      data = curve,
      aes(x, y),
      linewidth = 1
    ) +
    
    coord_equal() +
    theme_void() +
    labs(
      title = "Topelitz inscribed squares"
    )
  
  # Nariši vse rešitve
  if (!is.null(squares)) {
    
    for (i in seq_along(squares)) {
      
      sq <- rbind(
        squares[[i]],
        squares[[i]][1, ]
      )
      
      p <- p +
        
        # Square
        geom_path(
          data = sq,
          aes(x, y),
          colour = "red",
          linewidth = 1.2
        ) +
        
        # Four points on the curve
        geom_point(
          data = sq[1:4, ],
          aes(x, y),
          colour = "red",
          size = 2.5
        )
    }
  }
  
  p
}


# test run

f <- curve_blob(seed = 42)
squares <- find_squares(f, n = 200)

# -- all solutions
draw_solution(f, squares)
