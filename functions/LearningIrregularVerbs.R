##########################################
# 
# Irregular verbs - English
#
# Series:
# Little Useless-useful R functions #30
# Created: October 21, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
###########################################

learnVerbs <- function() {

        # populate the list of irregular verbs
        df <- data.frame(verb=c("drink","drive","cut","arise","fight","flee","become","begin","spin")
                         ,past=c("drank","drove","cut","arose","fought","fled","became","began","spun")
                         ,past_perfect=c("drunk","driven","cut","arisen","fought","fled","become","begun","spun")
        )
        exit <- "ex"
        correct <- 0
        wrong <- 0


        repeat {
          cat("\014") 
          randomWord <- df[sample(nrow(df), 1), ]
          randomWord[1]
          formW <- sample(c("past","past_perfect"), 1)
          promptText <- paste0("Find the __", formW, "__ form for the verb >>", toupper(randomWord[1,1]),"<< : ")
          promptText
          inputW <- readline(prompt=promptText)
          if (inputW == randomWord[1,formW]) {
            print("bravo")
            correct <- correct + 1
            barplot(table(c(replicate(correct, "correct"), replicate(wrong, "wrong"))))
            randomWord <- ""
        
          } else {
            print("naaah")
            wrong <- wrong +  1
            barplot(table(c(replicate(correct, "correct"), replicate(wrong, "wrong"))))
            randomWord <- ""
          }
          if (inputW == exit){
            wrong <- wrong -  1
            break
            barplot(table(c(replicate(correct, "correct"), replicate(wrong, "wrong"))))
          }
        }

}



learnVerbs()
