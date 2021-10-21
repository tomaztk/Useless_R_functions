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

        #
        # populate the list of irregular verbs
        #
        df <- data.frame(verb=c( "alight","arise","awake","be","bear","beat","become","beget","begin","bend","bereave","beseech",
                                 "bet","bid","bide","bind","bite","bleed","bless","blow","break","breed","bring","broadcast","build",
                                 "burn","burst","bust","buy","can","cast","catch","choose","cleave","clothe","come","cost","creep","crow",
                                 "cut","deal","dig","do","draw","dream","drink","drive","dwell","eat","fall","feed","feel","fight","find",
                                 "flee","fling","fly","forbid","forecast","forget","forsake","freeze","geld","get","gild","give","gnaw","go",
                                 "grind","grow","hang","have","hear","heave","hew","hide","hit","hold","hurt","keep","kneel","knit","know",
                                 "lay","lead","lean","leap","learn","leave","lend","let","lie","light","lose","make","may","mean","meet",
                                 "melt","mow","pay","pen","plead","prove","quit","read","rid","ride","ring","rise","run","saw","say","see",
                                 "seek","sell","send","set","sew","shake","shall","shear","shed","shine","shit","shoe","shoot","show","shred",
                                 "shrink","shut","sing","sink","sit","slay","sleep","slide","sling","slink","slit","smell","smite","sow",
                                 "speak","speed","spell","spend","spill","spin","spit","split","spoil","spread","spring","stand","steal",
                                 "stick","sting","stink","stride","strike","string","strive","swear","sweat","sweep","swell","swim","swing",
                                 "take","teach","tear","telecast","tell","throw","thrust","tread","wake","wear","weave","wed","weep","wet",
                                 "win","wind","wring","write")
                        ,past=c("alighted","arose","awoke","was","bore","beat","became","begot","began","bent","bereaved","besought","bet",
                                "bade","bade","bound","bit","bled","blessed","blew","broke","bred","brought","broadcast","built","burnt",
                                "burst","bust","bought","could","cast","caught","chose","cleft","clothed","came","cost","crept","crowed",
                                "cut","dealt","dug","did","drew","dreamt","drank","drove","dwelt","ate","fell","fed","felt","fought","found",
                                "fled","flung","flew","forbad","forecast","forgot","forsook","froze","gelded","got","gilded","gave","gnawed",
                                "went","ground","grew","hung","had","heard","heaved","hewed","hid","hit","held","hurt","kept","knelt",
                                "knitted","knew","laid","led","leant","leapt","learnt","left","lent","let","lay","lit","lost","made","might",
                                "meant","met","melted","mowed","paid","pent","pled","proved","quit","read","rid","rode","rang","rose","ran",
                                "sawed","said","saw","sought","sold","sent","set","sewed","shook","should","sheared","shed","shone","shit",
                                "shod","shot","showed","shred","shrank","shut","sang","sank","sat","slew","slept","slid","slung","slunk",
                                "slit","smelt","smote","sowed","spoke","sped","spelt","spent","spilt","spun","spat","split","spoilt","spread",
                                "sprang","stood","stole","stuck","stung","stank","strode","struck","strung","strove","swore","sweat","swept",
                                "swelled","swam","swung","took","taught","tore","telecast","told","threw","thrust","trod","woke","wore",
                                "wove","wed","wept","wet","won","wound","wrung","wrote")
                ,past_perfect=c( "alighted","arisen","awoken","been","borne","beaten","become","begotten","begun","bent","bereaved",
                                 "besought","bet","bidden","bided","bound","bitten","bled","blessed","blown","broken","bred","brought",
                                 "broadcast","built","burnt","burst","bust","bought","NONE","cast","caught","chosen","cleft",
                                 "clothed","come","cost","crept","crew","cut","dealt","dug","done","drawn","dreamt","drunk","driven","dwelt",
                                 "eaten","fallen","fed","felt","fought","found","fled","flung","flown","forbid","forecast","forgotten",
                                 "forsaken","frozen","gelded","got","gilded","given","gnawed","gone","ground","grown","hung","had","heard",
                                 "heaved","hewed","hidden","hit","held","hurt","kept","knelt","knitted","known","laid","led","leant","leapt",
                                 "learnt","left","lent","let","lain","lit","lost","made","NONE","meant","met","molten","mown",
                                 "paid","pent","pled","proven","quit","read","rid","ridden","rung","risen","run","sawn","said","seen","sought",
                                 "sold","sent","set","sewn","shaken","NONE","shorn","shed","shone","shit","shod","shot","shown",
                                 "shred","shrunk","shut","sung","sunk","sat","slain","slept","slid","slung","slunk","slit","smelt","smitten",
                                 "sown","spoken","sped","spelt","spent","spilt","spun","spat","split","spoilt","spread","sprung","stood",
                                 "stolen","stuck","stung","stunk","stridden","struck","strung","striven","sworn","sweat","swept","swollen",
                                 "swum","swung","taken","taught","torn","telecast","told","thrown","thrust","trodden","woken","worn","woven",
                                 "wed","wept","wet","won","wound","wrung","written")
        )
        
        # to exit the loop type: ex                
        exit <- "ex"
        correct <- 0
        wrong <- 0


        repeat {
          cat("\014")
          randomWord <- df[sample(nrow(df), 1), ]
          randomWord[1]
          formW <- sample(c("past","past_perfect"), 1)
          promptText <- paste0("Find the __", formW, "__ form for the verb >>", toupper(randomWord[1,1]),"<< : ")
          #promptText
          inputW <- toupper(readline(prompt=promptText))
          if (inputW == toupper(randomWord[1,formW])) {
            print("bravo")
            correct <- correct + 1
            barplot(table(as.character(c(replicate(correct, "correct"), replicate(wrong, "wrong")))), main = "correct vs. wrong", ylab="Number of words")
            randomWord <- ""
        
          } else {
            if (inputW == toupper(exit)){
              #wrong <- wrong -  1
              break
              #barplot(table(as.character(c(replicate(correct, "correct"), replicate(wrong, "wrong")))))
            } else {
            print("naaah")
            wrong <- wrong +  1
            barplot(table(as.character(c(replicate(correct, "correct"), replicate(wrong, "wrong")))), main = "correct vs. wrong", ylab="Number of words")
            randomWord <- ""
          }
        }
        }
}


# Run the function
learnVerbs()
