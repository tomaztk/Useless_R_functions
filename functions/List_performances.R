##########################################
#
# List Performances
#
#
# Series:
# Little Useless-useful R functions #45
# Created: January 06, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################
 

# 1 list of 3 sublists, each with 10 elements
myl <- list(sample (c('Yes','No'), size=10, replace=T, prob=c(.50,.50)),                     
                sample (c('Yes','No'), size=10, replace=T, prob=c(.80,.20)),
                sample (c('Yes','No'), size=10, replace=T, prob=c(.40,.60))
                )

#1 obs with 30 var
df <- data.frame(t(unlist(myl)))

# 1 var with 30 obs
df <- data.frame(unlist(myl))

# 3 obs with 10 var
df <- data.frame(do.call(rbind, myl))

# 3 var with 10 obs
df <- data.frame(t(do.call(rbind, myl)))


