##########################################
# 
# Generates useless histogram of folders
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


FolderHistogram <- function(directory){

input_directory <- "/users/tomazkastrun/Documents"
utils:::format.object_size(sum(file.info(list.files(path =input_directory, recursive = T, full.names = T))$size), "auto")

}

FolderHistogram(input_directory)
