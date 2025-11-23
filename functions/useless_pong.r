### Useless pong!
## with ball having its own trajectories and physics!
## and pad jitters (muahahaha)

useless_pong_tcl <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("Install tcltk package")
  }
  
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Useless Pong (tcltk edition)")

  W <- 500
  H <- 300
  
  canvas <- tcltk::tkcanvas(tt, width = W, height = H, bg = "black")
  tcltk::tkpack(canvas, expand = TRUE, fill = "both")
  

  paddle_width  <- 80
  paddle_height <- 10
  paddle_y      <- H - 30
  ball_radius   <- 8
  
  paddle <- tcltk::tkcreate(
    canvas, "rectangle",
    W/2 - paddle_width/2, paddle_y - paddle_height/2,
    W/2 + paddle_width/2, paddle_y + paddle_height/2,
    fill = "white", outline = "white"
  )
  
  ball <- tcltk::tkcreate(
    canvas, "oval",
    W/2 - ball_radius, H/2 - ball_radius,
    W/2 + ball_radius, H/2 + ball_radius,
    fill = "orange", outline = "orange"
  )
  
  tcltk::tkcreate(
    canvas, "text",
    W/2, 20,
    text = "Useless pong! Try to move your paddle with mouse",
    fill = "white"
  )
  

  state <- new.env(parent = emptyenv())
  state$x        <- W / 2
  state$y        <- H / 2
  state$vx       <- 3
  state$vy       <- -2
  state$ax       <- 0
  state$ay       <- 0.05  # fake gravity :D Newton will not hate us
  state$paddle_x <- W / 2
  state$running  <- TRUE
  

  tcltk::tkbind(canvas, "<Motion>", function(x, y) {
    x <- as.numeric(x)
    state$paddle_x <- 0.9 * state$paddle_x + 0.1 * x + stats::rnorm(1, 0, 20)
  })
  

  tcltk::tkbind(tt, "<space>", function() {
    state$running <- !state$running
  })
  

  repeat {
    exists_tt <- tryCatch(
      as.logical(tcltk::tkwinfo("exists", tt)),
      error = function(e) FALSE
    )
    if (!exists_tt) break
    
    if (isTRUE(state$running)) {
      # Physics: velocity gets small random nudges :D :D :D
      state$vx <- state$vx + state$ax + stats::rnorm(1, 0, 0.05)
      state$vy <- state$vy + state$ay
      
      speed <- sqrt(state$vx^2 + state$vy^2)
      maxv  <- 8
      if (speed > maxv) {
        state$vx <- state$vx * maxv / speed
        state$vy <- state$vy * maxv / speed
      }
      
      state$x <- state$x + state$vx
      state$y <- state$y + state$vy
      
      if (state$x - ball_radius < 0 || state$x + ball_radius > W) {
        state$vx <- -state$vx * runif(1, 0.8, 1.2)
        state$x  <- min(max(state$x, ball_radius), W - ball_radius)
      }
      if (state$y - ball_radius < 0) {
        state$vy <- -state$vy * runif(1, 0.8, 1.2)
        state$y  <- ball_radius
      }
    
      px <- state$paddle_x
      px <- min(max(px, paddle_width / 2), W - paddle_width / 2)
      
      tcltk::tkcoords(
        canvas, paddle,
        px - paddle_width/2, paddle_y - paddle_height/2,
        px + paddle_width/2, paddle_y + paddle_height/2
      )
      
      if (state$y + ball_radius >= paddle_y - paddle_height/2 &&
          state$y + ball_radius <= paddle_y + paddle_height &&
          abs(state$x - px) <= paddle_width/2) {
        
        state$vy <- -abs(state$vy) * runif(1, 0.5, 1.3)
        state$vx <- state$vx + stats::rnorm(1, 0, 1)
        state$y  <- paddle_y - paddle_height/2 - ball_radius
      }
      
      if (state$y - ball_radius > H) {
        state$x  <- runif(1, W * 0.2, W * 0.8)
        state$y  <- H / 3
        state$vx <- stats::rnorm(1, 0, 3)
        state$vy <- -abs(stats::rnorm(1, 3, 1))
      }
      
      # ball
      tcltk::tkcoords(
        canvas, ball,
        state$x - ball_radius, state$y - ball_radius,
        state$x + ball_radius, state$y + ball_radius
      )
    }
    
    # UI and FPS
    tcltk::tcl("update")
    Sys.sleep(0.02)  # ~50 FPS
  }
  
  invisible(NULL)
}


# runthe function
useless_pong_tcl()

