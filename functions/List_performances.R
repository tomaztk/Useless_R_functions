##########################################
#
# List to data.frame performance
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
# 250.000 elements is approx 46Mb in Size
# 2.500 elements for demo
for (i in 1:2500){ myl2[[i]] <- (cre_l(10,0,50))  } 
#####################
# different solutions
####################

sol1 <- NULL
sol1 <- data.frame(do.call(rbind, myl2))

sol2 <- NULL  #works for smaller lists
for (i in 1:length(myl2)){ sol2 <- rbind(sol2, data.frame(t(unlist(myl2[i])))) }

library (plyr)
sol3 <- NULL
sol3 <- ldply(myl2, sol3)


sol4 <- NULL
sol4 <- data.frame(t(sapply(myl2,c)))


sol5 <- NULL #works for smaller lists
sol5 <- data.frame(Reduce(rbind, myl2)) 


library(data.table)
sol6 <- NULL
#sol6 <- data.frame(rbindlist(lapply(myl2, data.frame)))
sol6 <- data.frame(t(rbindlist(list(myl2))))


# library(tidyverse)
# sol7 <- NULL
# sol7 <- myl2 %>% 
#           tibble::enframe() %>% 
#           unnest() 


# library(reshape2)
# library(dplyr)
# 
# column_names <- letters[1:10]
# myl2 %>% map_dfr(function(x) {x <- as.data.frame(x); colnames(x) <- column_names; x}) %>% 
#   as_tibble()

library(plyr)
sol7 <- NULL
sol7 <- ldply(myl2, c())

##################
# Test different solutions
###################

library(data.table)
library(plyr)
library(ggplot2) 

res <- summary(microbenchmark::microbenchmark(
    do_call_solution = {
          sol1 <- NULL
          sol1 <- data.frame(do.call(rbind, myl2))
    }, 
    for_loop_solution = { 
        sol2 <- NULL
        for (i in 1:length(myl2)){ sol2 <- rbind(sol2, data.frame(t(unlist(myl2[i])))) }
    },
     ldply_to_df = { 
          sol3 <- NULL 
          sol3 <- ldply(myl2, sol3)
     },
    ldply_to_c = {
      sol4 <- NULL
      sol4 <- ldply(myl2, c())
    },
    sapply = {
        sol5 <- NULL
        sol5 <- data.frame(t(sapply(myl2,c)))
        },
    recude = {
      sol6 <- NULL
      sol6 <- data.frame(Reduce(rbind, myl2))
    },
    data_table_rbindlist = {
      sol7 <- NULL
      sol7 <- data.frame(t(rbindlist(list(myl2))))
    },
    as_data_frame = {
      sol8 <- NULL
      sol8 <- data.frame(t(as.data.frame(myl2)))
    },
        times = 10L))



ggplot(res, aes(x=expr, y=(mean/1000/60))) + geom_bar(stat="identity", fill = "lightblue") +
     coord_flip() +
     labs(title = "Perfomance comparison", subtitle = "Converting list with 2.500 element to data.frame \n(less mean bettter)") +
     xlab("Methods") + ylab("Conversion time (s)") +
     theme_light() +
     geom_text(aes(label=(round(mean/1000/60*1.000,3))))

