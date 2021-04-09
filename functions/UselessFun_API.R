# UselessFun_API.R

#* Vrne sporocilo iz vhoda
#* @param msg Sporocilo za prikaz
#* @get /sporocilce
function(msg1="", msg2="") {
  list(paste0("Sporocilce 1 je: '", msg1, "'", " in  sporocilce 2 je: '", msg2, "'"))
}

#* Izrise histogram
#* @serializer png
#* @get /diagram
function() {
  library(ggplot2)
  rand <- rnorm(100)
  hist(rand)
}

#* Vrne zmnozek dveh cifr
#* @param a Prva stevka za multiply
#* @param b Druga stevka za multiply
#* @get /produkt
function(a="", b="") {
  res <- as.numeric(a) * as.numeric(b)
  list(paste0("The result is ", res))
}


#* Vrne zmnozek dveh cifr
#* @param a Prva stevka za multiply
#* @param b Druga stevka za multiply
#* @post /prod
function(a, b) {
   as.numeric(a) * as.numeric(b)
}



