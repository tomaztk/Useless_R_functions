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

#stopIfnot()
input_numbers <- c(23,22,25)
stopifnot(is.numeric(input_numbers)) #all TRUE

input_statements <- c(10==10, 22 > 10)
stopifnot(input_statements) #all TRUE

input_pi <- 3.15 #this is not a PI number!
stopifnot(all.equal(pi, input_pi)) #Error returned


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
 
for(input in input_list) {
   print(paste("TryCatch function of", input, "=", TC_fun(input)))
}

#########################
# We want to embedd a
# for/IF/while loop to 
# create a control flow
#########################

power_calculation <- function(x,y){
  tryCatch(
    expr = {
      message(x**y)
      message("Successfully calculated x to power of y.")
    },
    error = function(e){
      message('Caught an error!')
      print(e)
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
    },
    finally = {
      message('... Program execution finished ...')
    }
  )    
}

power_calculation(10,3)


get_numbers <- function(a,b){

#  stopifnot(is.numeric(a))
#  stopifnot(is.numeric(b))
  
  if (!is.numeric(a)) {
    warning("a must be a numeric")
  }
  if (!is.numeric(b)) {
    warning("b must be a numeric")
  }

  stopifnot(is.numeric(a) & is.numeric(b)) 
    power_calculation(a,b)
  
}

get_numbers("a",2)
get_numbers(5,2)




