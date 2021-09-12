##########################################
# 
# Year Progress in R
#
# Series:
# Little Useless-useful R functions #25
# Created: September 10, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################

# spinningCursor <- function(){
#   cursor <- c("\\","|","/","-")
#   #for (i in 1:10){
#     for(ii in 1:4){
#     Sys.sleep(0.05)
#     cat("\r",cursor[ii])
#    # }
#   }
# }


yearProgress <- function(){
          year <- format(Sys.Date(), format="%Y")
          difference <- as.integer(Sys.Date()-as.Date(paste0(year, "-01-01")))/365

          WidthBar <- 50
          LenProgress <- 40
          cursor <- c("\\","|","/","-")
          
          for (LenStep in 1:LenProgress) {
            step <- round((LenStep/LenProgress * (WidthBar-5))*difference)
            charSpinningCursor <- (LenStep %% 4)+1
            
            text <- sprintf('%s |%s%s % 3s%%', 
                            cursor[charSpinningCursor],
                            strrep('▓', step),
                            strrep('░', WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
                            
                            )
            
            cat("Yearly progress so far ",year, "...\n")
            cat(text)  
            Sys.sleep(0.2)
            cat(if (LenStep == LenProgress) 
                '\n' else '\014'
                )
           }
}

# Run function
yearProgress()


