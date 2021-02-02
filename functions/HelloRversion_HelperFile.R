##########################################
# 
# SHelper file for HelloRVersion
# Script for outputing R version
#
# Series:
# Little Useless-useful R functions #17
# Created: February 02, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

r1 <- paste("H   H EEEEE L     L      OOO       RRRR    !! 
H   H E     L     L     O   O      R   R   !! 
HHHHH EEEEE L     L     O   O      RRRR    !! 
H   H E     L     L     O   O      R   R     
H   H EEEEE LLLLL LLLLL  OOO       R   R   !!
---------------------------------------------
          | ver: ")

serialized_r1 <- r1 %>% 
  serialize(NULL) %>% 
  as.character() %>%
  paste0(collapse = "")

serialized_r1

serialized_r1 <- "580a000000030003060300030500000000055554462d38000000100000000100040009000001284820202048204545454545204c20202020204c2020202020204f4f4f2020202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f20202020202052202020522020202121200a4848484848204545454545204c20202020204c20202020204f2020204f20202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f202020202020522020205220202020200a4820202048204545454545204c4c4c4c4c204c4c4c4c4c20204f4f4f20202020202020522020205220202021210a2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d0a202020202020202020207c207665723a20"





r2 <- paste("                                  __
                               _.-~  )
                    _..--~~~~,'   ,-/     _
                 .-'. . . .'   ,-','    ,' )
               ,'. . . _   ,--~,-'__..-'  ,'
             ,'. . .  (@)' ---~~~~      ,'
            /. . . . '~~             ,-'
           /. . . . .             ,-'
          ; . . . .  - .        ,'
         : . . . .       _     /
        . . . . .          `-.:
       . . . ./  - .          )
      .  . . |  _____..---.._/ 
~---~~~~----~~~~             ~~
      | ver: ")

serialized_r2 <- r2 %>% 
  serialize(NULL) %>% 
  as.character() %>%
  paste0(collapse = "")

serialized_r2
