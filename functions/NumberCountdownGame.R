##########################################
# 
# Script that finds a solution for 
# Number countdown game 
#
# Series:
# Little Useless-useful R functions #16
# Created: January 9, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################


number_pool <- c(1:11, 25, 50, 75, 100, 200)
six_nm <- sample(number_pool, 6, replace=FALSE)
ran_3digit_num <- sample(100:999,1)

oper <- c("+","*","/","-")
para <- c("(",")")
step_counter <- 0
res <- 0

# 1000 runs each ?
while (ran_3digit_num != res) {
  oper1 <- sample(oper,1,replace=TRUE)
  #2 num
  six_nm_2_1 <- sample(six_nm,1,replace=FALSE)
  six_nm_2_2 <- sample(six_nm,1,replace=FALSE)
  oper1 <- sample(oper,1,replace=TRUE)  
  
  for2 <- paste0(six_nm_2_1,oper1,six_nm_2_2)
  step_counter <- step_counter + 1
  res <- eval(parse(text=for2))

  #3 num
  six_nm_3_1 <- sample(six_nm,1,replace=FALSE)
  six_nm_3_2 <- sample(six_nm,1,replace=FALSE)
  six_nm_3_3 <- sample(six_nm,1,replace=FALSE)
  oper2_1 <- sample(oper,1,replace=TRUE)
  oper2_2 <- sample(oper,2,replace=TRUE)
  
  for3 <- paste0(six_nm_3_1,oper2_1,six_nm_3_2,oper2_2,six_nm_3_3)
  step_counter <- step_counter + 1
  res <- eval(parse(text=for3))
  
  
}


solve_inner <- function(nums, value, expr = value, target, verbose = FALSE) {
  
  if (value == target) {
    if (verbose) {
      cat(expr, "\n")
    }
    return(expr)
  } else if (length(nums) == 0L) {
    return()
  }
  
  all_res <- c()
  
  for (op in c('+', '-', '*', '/')) {
    for(num_idx in seq_along(nums)) {
      
      new_value <- switch(
        op,
        '+' = nums[num_idx] + value,
        '-' = nums[num_idx] - value,
        '*' = nums[num_idx] * value,
        '/' = ifelse(nums[num_idx] %% value == 0L, nums[num_idx] %/% value, NA_integer_)
      )
      
      if (!is.na(new_value)) {
        res <- solve_inner(
          nums    = nums[-num_idx], 
          value   = new_value, 
          expr    = paste("(", nums[num_idx], ' ', op, ' ', expr, ")", sep = ''),
          target  = target,
          verbose = verbose
        )
        if (!is.null(res)) { all_res <- c(all_res, res) }
      }    
      
      if (op %in% c('-', '/')) {
        new_value <- switch(
          op,
          '-' = value - nums[num_idx],
          '/' = ifelse(value && nums[num_idx] == 0L, value %/% nums[num_idx], NA_integer_)
        )
        
        if (!is.na(new_value)) {
          res <- solve_inner(
            nums    = nums[-num_idx], 
            value   = new_value, 
            expr    = paste("(", expr, ' ', op, ' ', nums[num_idx], ")", sep = ''),
            target  = target,
            verbose = verbose
          )
          if (!is.null(res)) { all_res <- c(all_res, res) }
        }    
      }
    } # num_idx loop
  }   # op loop
  
  all_res 
}



solve_countdown <- function(nums, target, verbose = FALSE) {
  nums   <- as.integer(nums)
  target <- as.integer(target)
  seq_along(nums) %>% 
    purrr::map(~solve_inner(nums[-.x], value = nums[.x], target = target, verbose = verbose)) %>%
    purrr::flatten_chr() %>%
    unique()
}


nums   <- c(50, 9, 4, 5, 9, 3)
target <- 952 
solve_countdown(nums, target)
