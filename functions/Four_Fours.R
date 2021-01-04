##########################################
# 
# Four Fours - Mathematical puzzle
# 
# Series:
# Little Useless-useful R functions #16
# Created: January  05, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# Changelog: 
#  
###########################################



oper <- c("+","*", "-", "/")
para <- c("(",")")

#i <- 0
for (i in 0:2) {
    step_counter <- 0
    res <- i + 1
      while (i != res) {
      oper3 <- sample(oper,4,replace=FALSE)
      for44 <- paste0("4",oper3[1],"4",oper3[2],"4",oper3[3],"4")
      
      #adding paranthesis
      # dif =  3, 5, 7 
      stopit <- FALSE
      while (!stopit){
        pos_par <<- sort(sample(1:7,2)) 
        nn <- pos_par[1]
        mm <- pos_par[2]
        rr <<- abs(nn-mm)
        
        if (rr == 4 | rr == 5 ){
          stopit <- TRUE
          
        }
      }
      
      for44 <- paste0(substr(for44, 1, nn-1), "(", substr(for44, nn, nchar(for44)), sep = "")
      for44 <- paste0(substr(for44, 1, mm-1+1), ")", substr(for44, mm+1, nchar(for44)), sep = "")
      
     # if (for44 ) like "(/" or "(-" or "(*" or "(+" -> switch to -> "/(" or "-("
      
      res <- eval(parse(text=for44))
      #print(paste0("vrednost: ",i,". formula: ", for44, ". rezultat: ",res ,collapse=NULL))
      step_counter <- step_counter + 1
      if (res==i){
        print(paste0("Value: ", res, " was found formula: ", for44, " with result: ", res, " and steps: ", step_counter, collapse=NULL))
      }
    
      }
    i <- i + 1
    }

