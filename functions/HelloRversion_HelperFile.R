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


### TRUE

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


### FALSE
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
