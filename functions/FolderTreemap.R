##########################################
# 
# Generates useless Treemap of folders
# and size for given directory
# Series:
# Little Useless-useful R functions #9
# Created: December 30, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# Changelog: 
#  
###########################################

#install.packages("treemap")
library(treemap)


FolderTreemap <- function(directory){
  df <- NULL
  aa <- list.files(directory,full.names=TRUE)
  dirs <- aa[file.info(aa)$isdir]
  for (i in 1:length(dirs)){
    name_f <- basename(dirs[i])
    size_f <- sum(file.info(list.files(path=dirs[i], recursive = T, full.names = T))$size)
    df <- rbind(df, data.frame(size=size_f/(1024*1024), folder=name_f))
  }
  totalSize <- as.integer(sum(df$size))
  p <- treemap(df,
               index=c("folder"),
               vSize="size",
               type="index",
               palette = "Set2",
               bg.labels=c("white"),
               title = paste0("Total size of ",directory, " is: ", totalSize, " MiB.", collapse=NULL),
               align.labels=list(
                 c("center", "center"), 
                 c("right", "bottom")
               )  
  )            
}

input_directory <- "/users/tomazkastrun/Documents/Github"
FolderTreemap(input_directory)







