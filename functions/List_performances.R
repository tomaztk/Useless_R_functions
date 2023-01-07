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
myl <- list(sample (c('Yes','No'), size=30, replace=T, prob=c(.50,.50)),                     
                sample (c('Yes','No'), size=30, replace=T, prob=c(.80,.20)),
                sample (c('Yes','No'), size=30, replace=T, prob=c(.40,.60))
                )

#1 obs with 30 var
df <- data.frame(t(unlist(myl)))

# 1 var with 30 obs
df <- data.frame(unlist(myl))

# 3 obs with 10 var
df <- data.frame(do.call(rbind, myl))

# 3 var with 10 obs
df <- data.frame(t(do.call(rbind, myl)))


######################
# making bigger list##
######################

cre_l <- function(len,start,end){ return(round(runif(len,start,end),8)) }
myl2 <- list()
for (i in 1:250000){ myl2[[i]] <- (cre_l(10,0,50))  } #element is approx 46Mb in Size


# different three different solutions
sol1 <- NULL
sol1 <- data.frame(do.call(rbind, myl2))

sol2 <- NULL
for (i in 1:length(myl2)){ sol2 <- rbind(sol2, data.frame(t(unlist(myl2[i])))) }

sol3 <- NULL
library (plyr)
sol3 <- ldply(myl2, sol3)


sol4 <- NULL
sol4 <- data.frame(t(sapply(myl2,c)))


# sol5 <- NULL
# sol5 <- data.frame(Reduce(rbind, myl2))


sol6 <- NULL
library(data.table)
sol6 <- rbindlist(myl2, use.names=TRUE, fill=TRUE, idcol=TRUE)

?rbindlist

sol7 <- NULL
library(tidyverse)
l_tib <- myl2 %>% 
  unlist(recursive = FALSE) %>% 
  enframe() %>% 
  unnest()
l_tib


sol8 <- NULL
sol8 <- dplyr::bind_rows(myl2)
sol8 <- purrr::map_df(myl2, dplyr::bind_rows)
sol8 <- purrr::map_df(myl2, ~.x)


sol9 <- NULL
library(purrr)
sol9 <- map_df(myl2, ~.x)

sol10 <- NULL
library(reshape)
l <- melt(myl2)
sol10 <- dcast(l, value ~ L1)


sol11 <- NULL
lov = unlist(myl2, recursive = FALSE )
library(plyr)
sol11 <- ldply(lov)

##################
# Test different solutions
###################
sol1  <- NULL
sol2  <- NULL
sol3  <- NULL
sol4  <- NULL
microbenchmark::microbenchmark(
    #solut1
    sol1 <- data.frame(do.call(rbind, myl2)), 
    #solut2
    #for (i in 1:length(myl2)){ sol2 <- rbind(sol2, data.frame(t(unlist(myl2[i])))) },
    #solut3
    sol3 <- ldply(myl2, sol3),
    #solut4
    sol4 <- data.frame(t(sapply(myl2,c))),
    
                               times = 5L)

#######
# Results


