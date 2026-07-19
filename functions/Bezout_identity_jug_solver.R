##########################################
# 
# Useless useful R functions
# Bézout's identity and BFS solver
#
# Series:
# Little Useless-useful R functions #95
# Created: July 12, 2026
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.1

###########################################


solve_jugs <- function(caps = c(16, 11, 7), start = c(16, 0, 0), goal = c(8, 8, 0)) {
  # BFS over all (a, b, c) states
  # Each state is a named integer vector of water = amount is each jug or ?????
  
  queue   <- list(list(state = start, path = list(start)))
  visited <- list()
  key     <- function(s) paste(s, collapse = "-")
  
  while (length(queue) > 0) {
    node  <- queue[[1]]
    queue <- queue[-1]
    s     <- node$state
    
    if (isTRUE(all(s == goal))) return(node$path)
    if (!is.null(visited[[key(s)]])) next
    visited[[key(s)]] <- TRUE
    
    n <- length(s)
    for (from in 1:n) {
      for (to in 1:n) {
        if (from == to || s[from] == 0 || s[to] == caps[to]) next
        
        pour    <- min(s[from], caps[to] - s[to])
        new_s   <- s
        new_s[from] <- s[from] - pour
        new_s[to]   <- s[to]   + pour
        
        if (is.null(visited[[key(new_s)]])) {
          queue <- c(queue, list(list(
            state = new_s,
            path  = c(node$path, list(new_s))
          )))
        }
      }
    }
  }
  NULL  # no solution; add message or smht :)
}

solution <- solve_jugs()

 
for (step in solution) {
  cat(sprintf("  %-4d  %-4d  %-4d\n", step[1], step[2], step[3]))
}
  