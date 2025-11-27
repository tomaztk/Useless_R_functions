getwd()
setwd("/Users/tomazkastrun/Desktop")

fourier_selfie <- function(
    path,
    mode = c("lowpass","bandpass","directional"),
    levels = c(0.03, 0.06, 0.10, 0.18, 0.30, 1.00),
    # level params per mode:
    band = c(0.08, 0.20),     # for bandpass: inner, outer radius (fractions of Nyquist)
    angle_deg = 0,            # for directional: center angle (degrees, 0 = horizontal)
    ang_width_deg = 15,       # angular half-width for directional pass
    max_width = 256,          # downscale longer side for speed
    save_prefix = NULL        # optional folder/file prefix to save panels
){

  need <- c("magick","ggplot2","dplyr","tidyr","stringr","purrr")
  miss <- need[!need %in% installed.packages()[,"Package"]]
  if (length(miss)) install.packages(miss, quiet = TRUE)
  lapply(need, require, character.only = TRUE)
  
  mode <- match.arg(mode)
  

  fftshift <- function(m){
    nr <- nrow(m); nc <- ncol(m)
    r <- (floor(nr/2)+1):nr; r2 <- 1:floor(nr/2)
    c <- (floor(nc/2)+1):nc; c2 <- 1:floor(nc/2)
    m[c(r, r2), c(c, c2)]
  }
  ifftshift <- function(m){
    nr <- nrow(m); nc <- ncol(m)
    r <- (nr - floor(nr/2) + 1):nr; r2 <- 1:(nr - length((nr - floor(nr/2) + 1):nr))
    c <- (nc - floor(nc/2) + 1):nc; c2 <- 1:(nc - length((nc - floor(nc/2) + 1):nc))
    m[c(r, r2), c(c, c2)]
  }
  

  img <- magick::image_read(path)
  # keep aspect; cap longest side
  info <- magick::image_info(img)
  scale <- if (max(info$width, info$height) > max_width)
    sprintf("%dx", max_width) else NULL
  if (!is.null(scale)) img <- magick::image_scale(img, scale)
  
  img_gray <- img |>
    magick::image_convert(colorspace = "Gray") |>
    magick::image_data()   # 3D raw array [chan, x, y]
  # convert to numeric matrix in [0,1], orientation: rows=Y (top->bottom), cols=X
  M <- as.integer(dim(img_gray)[3]) # height
  N <- as.integer(dim(img_gray)[2]) # width
  mat <- matrix(
    as.integer(img_gray[1, , ]) / 255,
    nrow = M, ncol = N, byrow = FALSE
  )
  
  # --- FFT and spectrum -------------------------------------------------------
  F <- fft(mat)                    # unshifted
  Fsh <- fftshift(F)               # shift for pretty spectrum
  mag_spec <- log1p(Mod(Fsh))
  mag_spec <- mag_spec / max(mag_spec)
  
  # --- coordinate grids -------------------------------------------------------
  # center indices (frequency origin at center)
  u <- matrix(rep(seq_len(N) - (N/2 + 0.5), each = M), nrow = M) # cols
  v <- matrix(rep(seq_len(M) - (M/2 + 0.5), times = N), nrow = M) # rows
  r <- sqrt(u^2 + v^2)
  r_max <- max(r)
  # angle in radians, range (-pi, pi]
  theta <- atan2(v, u)
  
  # --- mask builders ----------------------------------------------------------
  # Normalize radius to [0,1] relative to Nyquist-ish (edge of grid)
  r_norm <- r / r_max
  
  build_mask <- function(level){
    if (mode == "lowpass") {
      cutoff <- level           # fraction of radius
      mask <- (r_norm <= cutoff)
    } else if (mode == "bandpass") {
      r1 <- band[1]; r2 <- band[2]
      # let "levels" scale band thickness (fun slider): expand outer radius
      r2s <- pmin(1, r2 * level / max(levels))
      mask <- (r_norm >= r1 & r_norm <= r2s)
    } else { # directional
      # pass wedge centered at angle_deg with half-width ang_width_deg * level
      a0 <- angle_deg * pi/180
      w  <- (ang_width_deg * level) * pi/180
      # shortest angular distance
      dang <- atan2(sin(theta - a0), cos(theta - a0))
      mask <- (abs(dang) <= w)
      # also include the symmetric opposite direction (±π) to keep real output decent
      dang2 <- atan2(sin(theta - (a0 + pi)), cos(theta - (a0 + pi)))
      mask <- mask | (abs(dang2) <= w)
      # gently taper near DC so we keep the image’s mean
      mask[r_norm < 0.01] <- TRUE
    }
    storage.mode(mask) <- "logical"
    mask
  }
  
  # --- reconstruct at each level ---------------------------------------------
  recs <- purrr::map(levels, function(lev){
    mask <- build_mask(lev)
    Fmasked_sh <- Fsh * mask
    Fmasked <- ifftshift(Fmasked_sh)

    rec_vec <- Re(fft(Fmasked, inverse = TRUE)) / length(Fmasked)
    rec     <- matrix(rec_vec, nrow = M, ncol = N, byrow = FALSE)
    
    list(level = lev, img = rec, mask = mask)
  })
  
  # --- tidy for ggplot --------------------------------------------------------
  mat_df <- function(mat, tag) {
    # Convert to numeric matrix no matter what
    if (is.null(dim(mat))) {
      len <- length(mat)
      n <- floor(sqrt(len))
      mat <- matrix(mat, nrow = n)
    }
    tibble::tibble(
      x = rep(seq_len(ncol(mat)), each = nrow(mat)),
      y = rep(seq_len(nrow(mat)), times = ncol(mat)),
      val = as.vector(mat),
      panel = tag
    )
  }
  
  
  original_df <- mat_df(mat, "original")
  spec_df     <- mat_df(mag_spec, "spectrum (log-scaled)")
  
  rec_dfs <- purrr::imap_dfr(recs, function(x, i){
    tag <- sprintf("%s: level %.2f", mode, x$level)
    mat_df(x$img, tag)
  })
  
  # order panels: original, spectrum, then reconstructions
  rec_labels <- unique(rec_dfs$panel)
  all_df <- dplyr::bind_rows(original_df, spec_df, rec_dfs) |>
    dplyr::mutate(panel = factor(panel, levels = c("original", "spectrum (log-scaled)", rec_labels)))
  
  # --- plot grid --------------------------------------------------------------
  p <- ggplot(all_df, aes(x, y, fill = val)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_gradient(low = "black", high = "white") +
    scale_y_reverse() + coord_fixed(expand = FALSE) +
    facet_wrap(~panel, ncol = 3) +
    guides(fill = "none") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(
      title = sprintf("fourier_selfie(): %s filter", mode),
      subtitle = "Left: original • Middle: centered magnitude spectrum • Right: reconstructions at increasing levels",
      caption = "Useless R: turning faces into frequencies since right now."
    )
  
  if (!is.null(save_prefix)) {
    out_file <- sprintf("%s_fourier_selfie_%s.png", save_prefix, mode)
    ggsave(out_file, plot = p, width = 12, height = 8, dpi = 150)
    message("Saved figure to: ", out_file)
  }
  
  invisible(list(plot = p, reconstructions = recs))
}



fourier_selfie("/Users/tomazkastrun/Desktop/me.png", mode = "lowpass", levels = c(0.03, 0.06, 0.10, 0.18, 0.30, 1.00))



# Basic dreamy blur: keep only low frequencies
fourier_selfie("me.png", mode = "lowpass",
               levels = c(0.03, 0.06, 0.10, 0.18, 0.30, 1.00))

# Bandpass “edge donut”: show mid-frequency structure
fourier_selfie("me.png", mode = "bandpass",
               band = c(0.06, 0.25),
               levels = c(0.10, 0.20, 0.40, 0.60, 0.80, 1.00))

# Directional vibes: pass frequencies around 45° with broader wedges
fourier_selfie("me.png", mode = "directional",
               angle_deg = 45, ang_width_deg = 10,
               levels = c(0.2, 0.4, 0.7, 1.0))



res <- fourier_selfie("me.png", mode = "lowpass",
                      levels = c(0.03, 0.06, 0.10, 0.18, 0.30, 1.00))
res$plot 


fourier_selfie("me.png", mode = "bandpass",
               band = c(0.06, 0.25),
               levels = c(0.10, 0.20, 0.40, 0.60, 0.80, 1.00))$plot
