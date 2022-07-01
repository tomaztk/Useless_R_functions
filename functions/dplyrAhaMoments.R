#### Dplyr

library(dplyr)




# Case_when
x <- c(1,2,3,0,7,4,22,0,0)
case_stat <- function(x){
  
case_when(
  x == 0 ~ "NA",
  x >= 1 & x <= 4 ~ "Majke mi",
  x >= 5 & x <= 7 ~ "Yeah",
  TRUE   ~ "nikjer nič"
)

}
