#############################
#
# Exceptions handling
# overview of: stop(), warning(), on.exit(), warn(), error()
#
#############################



# stop()
test_stop <- function(min, max) {
  if (max < min) 
      stop("Upsii: max < min")
   print("We are good: max > min")
}

test_stop(22,10)
test_stop(10,22)



#warning(), message() and supressWarnings

test_warning <- function(min, max){
    if(max < min) {
      warning("Warning, warning. Max < Min")
    }
    if(max > min) {
      message("Well, max > min")
    }
}

test_warning(10,20)
test_warning(20,10)

suppressWarnings(test_warning(20,10))
suppressWarnings(test_warning(10,12))



# example of error!

# using list, instead of vector of character
# so output control is better
input_list <- list(1, 2, 3, 'R Rules', 10, -500, 1.23, 0, 1024)

for(inp in input_list) {
   print(paste("Power to the 2 of", inp, "is: ", inp**2))
}


# Try
# with silent ON
for(inp in input_list) {
  try(print(paste("Power to the 2 of", inp, "is: ", inp**2)), silent = TRUE)
}

# without silent
for(inp in input_list) {
  try(print(paste("Power to the 2 of", inp, "is: ", inp**2)))
}
  

# With TryCatch

TC_fun = function(x) {
     tryCatch(x**2,
              warning = function(w) {print(paste("negative argument", x)); 
                 -inp**x},
               error = function(e) {print(paste("non-numeric argument", x)); 
                 NaN}) 
}
 
for(input in input_list_try) {
   print(paste("TryCatch function of", input, "=", TC_fun(input)))
}


# We want to embedd this into a for/IF/while loop to create a control flow
# https://www.geeksforgeeks.org/condition-handling-in-r-programming/
# https://rstudio.github.io/r-manuals/r-lang/Exception-handling.html
# https://recology.info/2019/03/control-flow-exceptions/
  
  
