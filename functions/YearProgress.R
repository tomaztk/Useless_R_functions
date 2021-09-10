#
#
# year progress bar in R
#
#
yearProgress <- function(){
          diffr<- as.integer(Sys.Date()-as.Date("2021-01-01"))/365
          
          options(width = 60)
          n <- 40
          for (ii in 1:n) {
            #extra <- nchar('||100%')
            width <- options()$width
            step <- round(ii / n * (width - 5))*diffr
            text <- sprintf('|%s%s|% 3s%%', 
                            strrep('=', step),
                            strrep(' ', width - step - 5), round(ii / n * diffr*100)
                            )
            
            cat("Yearly progress so far ...\n")
            cat(text)
            Sys.sleep(0.05)
            cat(if (ii == n) '\n' else '\014')
           }
}

yearProgress()

