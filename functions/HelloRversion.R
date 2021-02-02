##########################################
# 
# Script for outputing R version
#
# Series:
# Little Useless-useful R functions #16
# Created: February 02, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#
###########################################

library(dplyr)


serialized_r1 <- "580a000000030003060300030500000000055554462d38000000100000000100040009000000e84820202048204545454545204c20202020204c2020202020204f4f4f2020202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f20202020202052202020522020202121200a4848484848204545454545204c20202020204c20202020204f2020204f20202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f202020202020522020205220202020200a4820202048204545454545204c4c4c4c4c204c4c4c4c4c20204f4f4f2020202020202052202020522020202121"
  
  
n <- R.version$version.string
n <- sub("R version ","",n)
n <- gsub("\\(.*?\\)", "", n)

  
unserialized <- serialized_r1 %>% 
    {substring(., seq(1, nchar(.), 2), seq(2, nchar(.), 2))} %>% 
    paste0("0x", .) %>% 
    as.integer %>% 
    as.raw %>% 
    unserialize()

cat(unserialized)
