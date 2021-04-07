##########################################
# 
# Useless API
# Series:
# Little Useless-useful R functions #23
# Created: April 8, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
###########################################


#install.packages("plumber", dependencies=TRUE)

old <- getwd()
path <- "Users/tomazkastrun/Documents/GitHub/Useless_R_functions/functions"

library(plumber)

plumb("Documents/GitHub/Useless_R_functions/functions/UselessFun_API.R") %>%
  pr_run(port=2908)

