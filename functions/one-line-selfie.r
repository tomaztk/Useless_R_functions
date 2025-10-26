getwd()
setwd("/Users/tomazkastrun/Desktop")

### one liner selfie
### failed failed failed!!!

one_liner_selfie <- function(
    path,
    max_width   = 512,    
    blur_sigma  = 1.2,     
    edge_q      = 0.85,    
    n_points    = 2000,    # cca (1k–6k)
    tsp_method  = c("nn","insertion","two_opt"),  # route heuristic
    close_loop  = FALSE,   
    smooth      = TRUE,  
    save_prefix = NULL,   
    seed        = 2908
){
  tsp_method <- match.arg(tsp_method)
  need <- c("magick","imager","ggplot2","dplyr","tibble","TSP")
  miss <- need[!need %in% installed.packages()[,"Package"]]
  if (length(miss)) install.packages(miss, quiet=TRUE)
  lapply(need, require, character.only = TRUE)
  
  set.seed(seed)

  img  <- magick::image_read(path)
  inf  <- magick::image_info(img)
  scale <- if (max(inf$width, inf$height) > max_width) sprintf("%dx", max_width) else NULL
  if (!is.null(scale)) img <- magick::image_scale(img, scale)
  gray <- magick::image_convert(img, colorspace="gray")
  cim  <- imager::magick2cimg(gray) |> imager::renorm(0,1)
  W <- dim(cim)[1]; H <- dim(cim)[2]
  
  sm   <- imager::isoblur(cim, blur_sigma)
  gx   <- imager::imgradient(sm, "x")
  gy   <- imager::imgradient(sm, "y")
  mag  <- sqrt(gx^2 + gy^2)
  
  thr  <- as.numeric(quantile(as.vector(mag), probs = edge_q, na.rm = TRUE))
  edges <- mag > thr
  
  arr  <- as.array(edges); if (length(dim(arr)) < 4) arr <- array(arr, dim = c(dim(arr),1,1))
  vals <- as.numeric(arr[,,1,1])
  x <- rep(seq_len(W), each = H)
  y <- rep(seq_len(H), times = W)
  df <- tibble::tibble(x=x, y=y, v=vals) |> dplyr::filter(v > 0)
  
  if (nrow(df) < 5) stop("Give more  edges.")
  arrm <- as.array(mag); if (length(dim(arrm)) < 4) arrm <- array(arrm, dim = c(dim(arrm),1,1))
  mvals <- as.numeric(arrm[,,1,1])
  idx_linear <- (df$y - 1L) * W + df$x
  df$m <- pmax(1e-6, mvals[idx_linear])
  keep <- min(n_points, nrow(df))
  pts  <- df[sample.int(nrow(df), keep, replace = FALSE, prob = df$m), c("x","y")]
  pts$xn <- (pts$x - 1) / (W - 1)
  pts$yn <- (pts$y - 1) / (H - 1)
  
  #  TSP heuristic - custom Dist object.
  # TSP
  d_fun <- function(i, j) {
    # Euclidean 
    dx <- pts$xn[i] - pts$xn[j]; dy <- pts$yn[i] - pts$yn[j]
    sqrt(dx*dx + dy*dy)
  }
  D <- stats::dist(cbind(pts$xn, pts$yn), method="euclidean")
  tsp <- TSP::TSP(D)
  
  tour <- switch(tsp_method,
                 "nn"        = TSP::solve_TSP(tsp, method = "nearest_insertion"), 
                 "insertion" = TSP::solve_TSP(tsp, method = "farthest_insertion"),
                 "two_opt"   = TSP::solve_TSP(tsp, method = "two_opt")
  )
  ord <- as.integer(tour)
  path <- pts[ord, c("x","y")]
  if (close_loop) path <- rbind(path, path[1,])
  
  if (smooth) {
    k <- max(3, round(nrow(path)*0.002))  
    f <- rep(1/k, k)
    pad <- function(v,k) c(rep(v[1], k-1), v, rep(v[length(v)], k-1))
    sx <- stats::filter(pad(path$x,k), f, sides=2)
    sy <- stats::filter(pad(path$y,k), f, sides=2)
    path$x <- as.numeric(sx[(k):(length(sx)-k+1)])
    path$y <- as.numeric(sy[(k):(length(sy)-k+1)])
  }
  

  p <- ggplot(path, aes(x, y)) +
    geom_path(linewidth = 0.6, lineend = "round", colour = "black") +
    coord_fixed(expand = FALSE) +
    scale_y_reverse() +
    theme_void() +
    theme(plot.margin = margin(20,20,20,20),
          plot.background = element_rect(fill="white", color=NA)) +
    labs(title = "one_liner_selfie(): single-stroke outline",
         subtitle = "Nearest-neighbor TSP over edge points",
         caption = "Useless usefull R functions — continuous-line plotter art")
  
  if (!is.null(save_prefix)) {
    ggsave(paste0(save_prefix, "_one_line.png"), p, width = 7, height = 7, dpi = 220)
  }
  
  if (interactive()) print(p)
  invisible(list(plot = p, path = path))
}


# Simple single-line drawing
one_liner_selfie("me2.png")
one_liner_selfie("me.png", n_points = 1000, edge_q = 0.9, close_loop = TRUE)
one_liner_selfie("me.png", n_points = 1200, edge_q = 0.8, smooth = TRUE)

#   tsp_method  = c("nn","insertion","two_opt")
one_liner_selfie("me.png",edge_q = 0.35, close_loop = TRUE,
                 n_points = 1000,smooth = TRUE,
                 tsp_method = "nn")

