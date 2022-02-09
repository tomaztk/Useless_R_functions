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


input_list <- list(1, 2, 4, -5, 'oh no', 10, 0)

for(inp in input_list) {
     print(paste("Power of 2 of", inp, "=", inp**2))
}


# Try
input_list_try <- list(1, 2, 4, -5, 'oh no', 10, 0)

# with silent ON
for(inp in input_list_try) {
  try(print(paste("Power of 2 of", inp, "=", inp**2)), silent = TRUE)
}

# without silent
for(inp in input_list_try) {
  try(print(paste("Power of 2 of", inp, "=", inp**2)))
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
  
  
