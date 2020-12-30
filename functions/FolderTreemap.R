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
  df <- data.frame(size = as.integer(NULL), folder=NULL)
  aa <- list.files(directory,full.names=TRUE)
  dirs <- aa[file.info(aa)$isdir]
  for (i in 1:length(dirs)){
    name_f <- basename(dirs[i])
    #utils:::format.object_size(sum(file.info(list.files(path =input_directory, recursive = T, full.names = T))$size), "auto")
    size_f <- sum(file.info(list.files(path=dirs[i], recursive = T, full.names = T))$size)
    df_temp <- data.frame(size=size_f/(1024*1024), folder=name_f)
    df <- rbind(df, df_temp)
  }
  df_out <<- df
  totalSize <- as.integer(sum(df_out$size))
  
  # plot(
  #   x = df$folder,
  #   y = df$size, 
  #   #x = df$size,
  #   type="h",
  #   main="Size of the subfolders",
  #   ylab="Size (in Mega-bytes)",
  #   border = "dark blue", 
  #   col = "light blue",
  #   labels = TRUE)
  
  p <- treemap(df_out,
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







