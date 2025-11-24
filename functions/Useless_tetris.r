
### USeless
### Tetris
###

tetris_tcltk <- function() {
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    stop("INstall package 'tcltk'.")
  }
  
  
  board_width  <- 10
  board_height <- 20
  cell_size    <- 20
  top_offset   <- 30    
  W <- board_width * cell_size
  H <- board_height * cell_size + top_offset + 10
  

  colors <- c(
    "#000000",  # 0 = empty (not used directly)
    "#00FFFF",  # I
    "#FFFF00",  # O
    "#AA00FF",  # T
    "#FFA500",  # L
    "#0000FF",  # J
    "#00FF00",  # S
    "#FF0000"   # Z
  )

  
  rotate_matrix <- function(m) t(apply(m, 2, rev))
  
  make_rotations <- function(mat) {
    rots <- list(mat)
    for (i in 1:3) {
      rots[[i + 1]] <- rotate_matrix(rots[[i]])
    }
 
    uniq <- list()
    keys <- character()
    for (r in rots) {
      k <- paste(r, collapse = "")
      if (!k %in% keys) {
        keys <- c(keys, k)
        uniq[[length(uniq) + 1]] <- r
      }
    }
    uniq
  }
  
  I_shape <- matrix(c(
    0,0,0,0,
    1,1,1,1,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  O_shape <- matrix(c(
    0,1,1,0,
    0,1,1,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  T_shape <- matrix(c(
    0,1,0,0,
    1,1,1,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  L_shape <- matrix(c(
    0,0,1,0,
    1,1,1,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  J_shape <- matrix(c(
    1,0,0,0,
    1,1,1,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  S_shape <- matrix(c(
    0,1,1,0,
    1,1,0,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  Z_shape <- matrix(c(
    1,1,0,0,
    0,1,1,0,
    0,0,0,0,
    0,0,0,0
  ), nrow = 4, byrow = TRUE)
  
  shapes <- list(
    make_rotations(I_shape),
    make_rotations(O_shape),
    make_rotations(T_shape),
    make_rotations(L_shape),
    make_rotations(J_shape),
    make_rotations(S_shape),
    make_rotations(Z_shape)
  )
  
   # game window
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "tcltk Tetris")
  canvas <- tcltk::tkcanvas(tt, width = W, height = H, bg = "black")
  tcltk::tkpack(canvas, expand = TRUE, fill = "both")
  tcltk::tkfocus(tt)
  tcltk::tkraise(tt)
  
  #envs
  state <- new.env(parent = emptyenv())
  state$board         <- matrix(0L, nrow = board_height, ncol = board_width)
  state$piece_x       <- 0L
  state$piece_y       <- 0L
  state$shape_idx     <- 1L
  state$rotation      <- 1L
  state$lines_cleared <- 0L
  state$running       <- TRUE
  state$game_over     <- FALSE
  state$drop_counter  <- 0L
  state$drop_interval <- 15L   # chngge the frames
  
 
  spawn_new_piece <- function() {
    state$shape_idx <- sample(seq_along(shapes), 1)
    state$rotation  <- 1L
    mat <- shapes[[state$shape_idx]][[state$rotation]]
    
    coords <- which(mat == 1, arr.ind = TRUE)
    min_row <- min(coords[, "row"])
    max_row <- max(coords[, "row"])
    min_col <- min(coords[, "col"])
    max_col <- max(coords[, "col"])
    
    piece_width <- max_col - min_col + 1
    target_left <- floor((board_width - piece_width) / 2) + 1
    
    state$piece_x <- target_left - (min_col - 1)
    state$piece_y <- 1 - (min_row - 1)
  }
  
  can_move_to <- function(px, py, rot) {
    mat <- shapes[[state$shape_idx]][[rot]]
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (mat[i, j] == 1) {
          bx <- px + j - 1
          by <- py + i - 1

          
          if (bx < 1 || bx > board_width || by > board_height) {
            return(FALSE)
          }

          if (by >= 1) {
            if (state$board[by, bx] != 0L) return(FALSE)
          }
        }
      }
    }
    TRUE
  }
  
  lock_piece <- function() {
    mat <- shapes[[state$shape_idx]][[state$rotation]]
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (mat[i, j] == 1) {
          bx <- state$piece_x + j - 1
          by <- state$piece_y + i - 1
          if (by >= 1 && by <= board_height &&
              bx >= 1 && bx <= board_width) {
            state$board[by, bx] <- state$shape_idx
          }
        }
      }
    }
 
    full <- which(apply(state$board, 1, function(r) all(r != 0)))
    if (length(full) > 0) {
      keep_rows <- setdiff(seq_len(board_height), full)
      new_rows  <- matrix(0L, nrow = length(full), ncol = board_width)
      state$board <- rbind(new_rows, state$board[keep_rows, , drop = FALSE])
      state$lines_cleared <- state$lines_cleared + length(full)
      state$drop_interval <- max(3L, 15L - state$lines_cleared %/% 5L)
    }

    
    spawn_new_piece()
    if (!can_move_to(state$piece_x, state$piece_y, state$rotation)) {
      state$game_over <- TRUE
    }
  }
  
 
  draw_board <- function() {
    tcltk::tkdelete(canvas, "all")
    
 
    score_text <- if (!state$game_over) {
      sprintf("tcltk Tetris  |  Lines: %d", state$lines_cleared)
    } else {
      sprintf("GAME OVER | Lines: %d  (Press R to restart)", state$lines_cleared) ## not-a-worka!
    }
    
    tcltk::tkcreate(
      canvas, "text",
      W / 2, 15,
      text = score_text,
      fill = "white"
    )
     
    for (row in 1:board_height) {
      for (col in 1:board_width) {
        val <- state$board[row, col]
        x0 <- (col - 1) * cell_size
        y0 <- top_offset + (row - 1) * cell_size
        fill <- if (val == 0L) "#111111" else colors[val + 1]
        tcltk::tkcreate(
          canvas, "rectangle",
          x0, y0, x0 + cell_size, y0 + cell_size,
          fill = fill, outline = "#222222"
        )
      }
    }
    
 
    if (!state$game_over) {
      mat <- shapes[[state$shape_idx]][[state$rotation]]
      for (i in 1:nrow(mat)) {
        for (j in 1:ncol(mat)) {
          if (mat[i, j] == 1) {
            col <- state$piece_x + j - 1
            row <- state$piece_y + i - 1
            if (row >= 1 && row <= board_height &&
                col >= 1 && col <= board_width) {
              x0 <- (col - 1) * cell_size
              y0 <- top_offset + (row - 1) * cell_size
              fill <- colors[state$shape_idx + 1]
              tcltk::tkcreate(
                canvas, "rectangle",
                x0, y0, x0 + cell_size, y0 + cell_size,
                fill = fill, outline = "#444444"
              )
            }
          }
        }
      }
    }
    
    # game restart
    if (state$game_over) {
      tcltk::tkcreate(
        canvas, "text",
        W / 2, H / 2,
        text = "GAME OVER\nPress R to restart\nEsc to quit",
        fill = "red"
      )
    }
  }
  
 
  move_piece <- function(dx, dy) {
    if (state$game_over || !state$running) return()
    px <- state$piece_x + dx
    py <- state$piece_y + dy
    if (can_move_to(px, py, state$rotation)) {
      state$piece_x <- px
      state$piece_y <- py
    }
  }
  
  rotate_piece <- function() {
    if (state$game_over || !state$running) return()
    rots <- length(shapes[[state$shape_idx]])
    new_rot <- (state$rotation %% rots) + 1L
    if (can_move_to(state$piece_x, state$piece_y, new_rot)) {
      state$rotation <- new_rot
    }
  }
  
  hard_drop <- function() {
    if (state$game_over || !state$running) return()
    while (can_move_to(state$piece_x, state$piece_y + 1L, state$rotation)) {
      state$piece_y <- state$piece_y + 1L
    }
  }
  
  restart_game <- function() {
    state$board         <- matrix(0L, nrow = board_height, ncol = board_width)
    state$lines_cleared <- 0L
    state$running       <- TRUE
    state$game_over     <- FALSE
    state$drop_interval <- 15L
    state$drop_counter  <- 0L
    spawn_new_piece()
  }
  
  # Key
  tcltk::tkbind(tt, "<Left>",  function() move_piece(-1L, 0L))
  tcltk::tkbind(tt, "<Right>", function() move_piece( 1L, 0L))
  tcltk::tkbind(tt, "<Down>",  function() move_piece( 0L, 1L))
  tcltk::tkbind(tt, "<Up>",    function() rotate_piece())
  tcltk::tkbind(tt, "<space>", function() hard_drop())
  tcltk::tkbind(tt, "p",       function() state$running <- !state$running)
  tcltk::tkbind(tt, "P",       function() state$running <- !state$running)
  tcltk::tkbind(tt, "r",       function() restart_game())
  tcltk::tkbind(tt, "R",       function() restart_game())
  tcltk::tkbind(tt, "<Escape>", function() {
    # Close window on Esc
    tcltk::tkdestroy(tt)
  })
  
  ## start of the game
  restart_game()
  
  repeat {
   
    exists_tt <- tryCatch(
      as.logical(tcltk::tkwinfo("exists", tt)),
      error = function(e) FALSE
    )
    if (!exists_tt) break
    
    if (!state$game_over && state$running) {
      state$drop_counter <- state$drop_counter + 1L
      if (state$drop_counter >= state$drop_interval) {
        state$drop_counter <- 0L
        if (can_move_to(state$piece_x, state$piece_y + 1L, state$rotation)) {
          state$piece_y <- state$piece_y + 1L
        } else {
          lock_piece()
        }
      }
    }
    
    draw_board()
    tcltk::tcl("update")
    Sys.sleep(0.03)  # ~33 FPS
  }
}



# run tetris:
tetris_tcltk()

