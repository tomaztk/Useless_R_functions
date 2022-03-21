
##########################################
# 
# Benchmarking vectors and data.frames
# on simple MapReduce problem
#
# Series:
# Little Useless-useful R functions #35
# Created: March 21, 2022
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


library(rbenchmark)
library(purrr)
library(dplyr)

len <- 10000000  # 10^6
lett <- c(LETTERS[1:20])

set.seed(2908)
a_vec <- do.call(paste0, c(as.list(sample(lett,len,replace=TRUE)), sep="")) # a vector

a_df <- data.frame(chr = paste0(sample(lett,len,replace=TRUE), sep="")) # a data frame


rbenchmark::benchmark(
"table with vector" = {
    res_table <- ""
    a_table <- table(strsplit(a_vec, ""))
    for (i in 1:length(names(a_table))) {
      key<- (names(a_table[i]))
      val<-(a_table[i])
      res_table <- paste0(res_table,key,val)
    }
},
"dplyr with data.frame" = {

res_dplyr <- a_df %>%
    count(chr, sort=TRUE) %>%
    mutate(res = paste0(chr, n, collapse = "")) %>%
    select(-chr, -n) %>%
    group_by(res)

res_dplyr[1,]
  
  
},
"purrr with data.frame" = {
  adf_table <- a_df %>% 
    map(~count(data.frame(x=.x), x))
  
    res_purrr <- ""
    for (i in 1:nrow(adf_table$chr)) {
      key<- adf_table$chr[[1]][i]
      val<- adf_table$chr[[2]][i]
      res_purrr <- paste0(res_purrr,key,val)
    }
  
},
  replications = 20, order = "relative"
)





