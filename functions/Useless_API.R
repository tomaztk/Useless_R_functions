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

### Get
# Url1: http://127.0.0.1:2908/sporocilce?msg1=aaaa&msg2=bbbb
# Url2: http://127.0.0.1:2908/diagram
# Url3: http://127.0.0.1:2908/produkt?a=2&b=3


### Post
#Url4: http://127.0.0.1:2908/prod?a=4&b=4
