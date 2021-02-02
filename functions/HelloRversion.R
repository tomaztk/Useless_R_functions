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

# Get some text
serialized_r1 <- "580a000000030003060300030500000000055554462d38000000100000000100040009000001284820202048204545454545204c20202020204c2020202020204f4f4f2020202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f20202020202052202020522020202121200a4848484848204545454545204c20202020204c20202020204f2020204f20202020202052525252202020202121200a4820202048204520202020204c20202020204c20202020204f2020204f202020202020522020205220202020200a4820202048204545454545204c4c4c4c4c204c4c4c4c4c20204f4f4f20202020202020522020205220202021210a2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d0a202020202020202020207c207665723a20"

# Get R version
vR<- trimws(gsub("\\(.*?\\)", "", sub("R version ","",R.version$version.string)))

  
unserialized_vR <- serialized_r1 %>% 
    {substring(., seq(1, nchar(.), 2), seq(2, nchar(.), 2))} %>% 
    paste0("0x", .) %>% 
    as.integer %>% 
    as.raw %>% 
    unserialize()

unserialized_vR <- paste0(unserialized_vR,vR,' |')


cat("\014") # or  cat("\f") if running on Windows
cat(unserialized_vR)
