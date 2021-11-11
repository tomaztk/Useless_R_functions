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
            
            #ASCII For MacOS
            # text <- sprintf('%s |%s%s % 3s%%', 
            #                 cursor[charSpinningCursor],
            #                 strrep('▓', step),
            #                 strrep('░', WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
            #                 
            #                 )
            
            
            #ASCII for Windows or MacOS
            # text <- sprintf('%s%s % 3s%%', 
            #                 #cursor[charSpinningCursor],
            #                 strrep('\U2593', step), # U+2593 dark  and U+2591 for light
            #                 strrep('\U2591', WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
            #                 
            # )
            
            # different version of ASCII 
            # text <- sprintf('%s%s % 3s%%',
            #                 #cursor[charSpinningCursor],
            #                 strrep("▮", step), # U+2593 █  and U+2591 for light
            #                 strrep("▯", WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
            #                 
            # )
            
            
            # different version of ASCII: look: [===>---]
            text <- sprintf('[%s>%s] % 3s%%',
                            #cursor[charSpinningCursor],
                            strrep("=", step), # U+2593 █  and U+2591 for light
                            strrep("-", WidthBar-step-5), round(LenStep/LenProgress*difference*100.00, digits=2)
                            
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


# store to file and create backup
timestamp <- format(Sys.Date(), "%Y%m%d")
filename <- paste("~/Desktop/YP-",timestamp,".txt",sep="")
con <- file(filename)
sink(con, append=FALSE)
cat(xx4)


# ToDO:

### Adding total number of days

### Adding total number of weeks

### Adding the month switching (from oct->nov)


