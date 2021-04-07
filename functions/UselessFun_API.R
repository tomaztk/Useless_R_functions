# UselessFun_API.R

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg1="", msg2="") {
  list(paste0("The message 1 is: '", msg1, "'", " and the Message 2 is: '", msg2, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the product of two numbers
#* @param a The first number to multiply
#* @param b The first number to multiply
#* @post /sum
function(a="", b="") {
  res <- as.numeric(a) * as.numeric(b)
  list(paste0("The result is ", res))
}