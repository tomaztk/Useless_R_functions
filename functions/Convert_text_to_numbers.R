#
# Old phone converted from text to numbers
#


let <- c("a","b","c","","d","e","f","","g","h","i","","j","k","l","","m","n","o","","p","q","r","s","t","u","v","","w","x","y","z")
mm <- matrix(let,  nrow = 8, ncol=4, byrow = TRUE, dimnames = list(
                                                            c("N2","N3","N4","N5","N6","N7","N8","N9"),
                                                            c("P.1","P.2","P.3","P.4")))

mm

tt <- "like"

for (i in 1:nchar(tt)){
  print(i)
  lt <- substr(tt,i,i)
  print(lt)
}



which(mm == "e", arr.ind = T)
which(mm == "l", arr.ind = T)
