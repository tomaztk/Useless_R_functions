##########################################
# 
# Letter frequency for numbers in a dataset
# Series:
# Little Useless-useful R functions #22
# Created: MARCH 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
###########################################


basLet <- c('one','two','three','four','five','six','seven','eight','nine','ten'
,'eleven','twelve','thirteen','fourteen','fifteen','sixteen','seventeen','eighteen','nineteen'
,'twenty','thirty','forty','fifty','sixty','seventy','eighty','ninety','one hundred')

basNum <- c(1:20,30,40,50,60,70,80,90,100)

df <- data.frame(num = basNum, let = as.character(basLet))
