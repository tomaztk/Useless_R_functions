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

yearProgress <- function(){
          difference <- as.integer(Sys.Date()-as.Date("2021-01-01"))/365
          year <- format(Sys.Date(), format="%Y")
          WidthBar <- 50
          LenProgress <- 40
          
          for (LenStep in 1:LenProgress) {
            step <- round((LenStep/LenProgress * (WidthBar-5))*difference)
              text <- sprintf('|%s%s % 3s%%', 
                            strrep('▓', step),
                            strrep('░', WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
                            )
            
            cat("Yearly progress so far ",year, "...\n")
            cat(text)
            Sys.sleep(0.1)
            cat(if (LenStep == LenProgress) 
                '\n' else '\014'
                )
           }
}

# Run function
yearProgress()

