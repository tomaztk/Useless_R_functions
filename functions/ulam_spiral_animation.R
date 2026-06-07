
getwd()
setwd("/Users/tomazkastrun/ulam")

ulam_spiral_animate <- function(
    N            = 49,
    file         = "ulam_spiral.gif",
    fps          = 10,
    width        = 700,
    height       = 740,
    build_pause  = 0.8,
    reveal_pause = 1.8
) {
  library(ggplot2)
  
  has_gifski <- requireNamespace("gifski", quietly = TRUE)
  has_magick <- requireNamespace("magick", quietly = TRUE)
  if (!has_gifski && !has_magick)
    stop("Install a renderer:\n  install.packages('gifski')  # recommended\n",
         "  install.packages('magick')  # alternative")
  
  if (!has_gifski) {
    fps <- c(1,2,4,5,10,20,25,50,100)[which.min(abs(c(1,2,4,5,10,20,25,50,100)-fps))]
  }
  
  new_gg <- packageVersion("ggplot2") >= "3.4.0"
  dr <- c(-1L,0L,1L,0L); dc <- c(0L,1L,0L,-1L)   # N E S W
  
  rv <- cv <- integer(N)
  r <- 0L; cc <- 0L; d <- 1L; step <- 1L; k <- 2L
  while (k <= N) {
    for (h in 1:2) {
      for (i in seq_len(step)) {
        r <- r+dr[d]; cc <- cc+dc[d]
        rv[k] <- r; cv[k] <- cc
        k <- k+1L; if (k>N) break
      }
      d <- (d%%4L)+1L; if (k>N) break
    }
    step <- step+1L
  }
  

  #learning about sieve
  ip <- rep(TRUE,N); ip[1] <- FALSE
  for (p in 2L:max(2L,as.integer(sqrt(N))))
    if (ip[p]) ip[seq.int(p*p,N,p)] <- FALSE
  
  n_primes <- sum(ip); n_comp <- N-n_primes; comp_idx <- which(!ip)
  
  px <- as.numeric(cv); py <- -as.numeric(rv)
  xl <- range(px)+c(-.85,.85); yl <- range(py)+c(-.85,.85)
  span  <- max(diff(range(px)),diff(range(py)))+1
  fsize <- max(2.0, 7.5/sqrt(span))
  

  pb       <- max(1L, round(build_pause  * fps))
  pd       <- max(1L, round(reveal_pause * fps))
  nf       <- N + pb + n_comp + pd
  
  BG <- "#07071a"
  C  <- list(curr="#ff6600", placed="#8888aa", prime="#ffd700",
             comp="#252540", eras="#ff2244", sbld="#1e3c5a", srev="#0b0b22")
  
  th <- theme_void() +
    theme(plot.background  = element_rect(fill=BG, color=NA),
          panel.background = element_rect(fill=BG, color=NA),
          legend.position  = "none",
          plot.margin      = margin(4,10,10,10))
  
  base_scales <- list(
    scale_colour_identity(),
    scale_size_identity(),
    scale_alpha_identity(),
    coord_fixed(xlim=xl, ylim=c(yl[1],yl[2]+.75), expand=FALSE),
    th
  )
  
  # Helper stuff
  make_seg_geom <- function(df, col, lw) {
    args <- list(data=df,
                 mapping=aes(x=x1,y=y1,xend=x2,yend=y2),
                 colour=col, lineend="round")
    if (new_gg) args$linewidth <- lw else args$size <- lw
    do.call(geom_segment, args)
  }
  

  out_abs   <- normalizePath(file, mustWork=FALSE)
  frame_dir <- file.path(dirname(out_abs), sprintf(".ulam_%d", as.integer(Sys.time())))
  dir.create(frame_dir, recursive=TRUE, showWarnings=FALSE)
  on.exit(unlink(frame_dir, recursive=TRUE), add=TRUE)
  
  frame_paths <- file.path(frame_dir, sprintf("%04d.png", seq_len(nf)))
  
  # title
  make_title <- function(f) {
    if      (f<=N)              sprintf("Step %d / %d", f, N)
    else if (f<=N+pb)           sprintf("Ulam Prime Spiral  \u00b7  %d\u00d7%d", N, N)
    else if (f<=N+pb+n_comp)    sprintf("Erasing composites  \u00b7  %d / %d", f-N-pb, n_comp)
    else sprintf("%d primes in 1\u2026%d  \u00b7  %.1f%% density  \u00b7  and diagonals are visible!", n_primes, N, 100*n_primes/N)
  }
  
    dots_printed <- 0L
  
  for (f in seq_len(nf)) {
    
    col_v <- rep(BG,N); alp_v <- rep(0,N); siz_v <- rep(fsize,N)
  
    if (f<=N) {
      col_v[seq_len(f)] <- C$placed; alp_v[seq_len(f)] <- 1
      col_v[f] <- C$curr;            siz_v[f] <- fsize*1.5
      
    } else if (f<=N+pb) {
      col_v <- ifelse(ip,C$prime,C$placed); alp_v[] <- 1
      
    } else if (f<=N+pb+n_comp) {
      gone <- f-N-pb;  now <- comp_idx[gone]
      col_v <- ifelse(ip,C$prime,C$comp); alp_v[] <- 1
      if (gone>1L) { col_v[comp_idx[seq_len(gone-1L)]] <- BG
      alp_v[comp_idx[seq_len(gone-1L)]] <- 0 }
      col_v[now] <- C$eras
      
    } else {
      col_v[ip] <- C$prime; alp_v[ip] <- 1
    }
    
    pts_f <- data.frame(x=px, y=py, n=seq_len(N), col=col_v, alp=alp_v, siz=siz_v)
    
    #
    ns <- if (f<2L) 0L else min(f-1L, N-1L)
    seg_layer <- if (ns>0L) {
      sc <- if (f<=N+pb) C$sbld else C$srev
      lw <- if (f<=N+pb) 0.5 else 0.22
      make_seg_geom(
        data.frame(x1=px[seq_len(ns)],y1=py[seq_len(ns)],
                   x2=px[seq_len(ns)+1L],y2=py[seq_len(ns)+1L]),
        col=sc, lw=lw)
    } else NULL
    
    title_f <- data.frame(x=mean(xl), y=yl[2]+.52, label=make_title(f))
    p_f <- ggplot() +
      seg_layer +
      geom_text(data=pts_f,
                aes(x=x,y=y,label=n,colour=col,
                    size=siz,alpha=alp),
                fontface="bold") +
      geom_text(data=title_f, aes(x=x,y=y,label=label),
                colour="#556677", size=3.2, 
                fontface="bold", hjust=0.5) +
      base_scales
    
    suppressMessages(
      ggsave(frame_paths[f], p_f,
             device="png", width=width, height=height, units="px",
             dpi=96, bg=BG)
    )
    
    # Progress bar :)
    pct <- as.integer(f/nf*10)
    if (pct > dots_printed) { cat("."); dots_printed <- pct }
  }
 
  if (!all(file.exists(frame_paths)))
    stop("Something went wrong")
  if (has_gifski) {
    gifski::gifski(frame_paths, gif_file=out_abs,
                   width=width, height=height,
                   delay=1/fps, loop=TRUE, progress=FALSE)
  } else {
    library(magick)
    image_write( image_animate(image_read(frame_paths), delay=round(100L/fps)), out_abs)
  }
}


#examples
  
ulam_spiral_animate(25,  "ulam_25.gif",  fps=10)
ulam_spiral_animate(49,  "ulam_49.gif",  fps=10)
ulam_spiral_animate(100, "ulam_100.gif", fps=10, width=860, height=900)
ulam_spiral_animate(49,  "ulam_slow.gif", fps=5, build_pause=2, reveal_pause=3)
