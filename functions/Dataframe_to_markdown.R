##########################################
# 
# create markdown table based on dataframe
#
# Series:
# Little Useless-useful R functions #51
# Created: March 23, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        
###########################################


# > iris[1:3,1:5]
# Result:
#> Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa

# Create function that will return a markdown script of table with
# data, ready for  Markdown document:

#> |Sepal.Length|Sepal.Width|Petal.Length|Petal.Width|Species|
#> |---|---|---|---|---|
#> |5.1|3.5|1.4|0.2|setosa|
#> |4.9|3|1.4|0.2|setosa|
#> |4.7|3.2|1.3|0.2|setosa|


df_2_MD <- function(your_df){
  
  cn <- as.character(names(your_df))
  headr <- paste0(c("", cn),  sep = "|", collapse='')
  sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), collapse=''),length(cn))), collapse ='')
  st <- "|"
    for (i in 1:nrow(your_df)){
      for(j in 1:ncol(your_df)){
        if (j%%ncol(your_df) == 0) {
          st <- paste0(st, as.character(your_df[i,j]), "|", "\n", "" , "|", collapse = '')
        } else {
        st <- paste0(st, as.character(your_df[i,j]), "|", collapse = '')
        }
      }
    }
  fin <- paste0(c(headr, sepr, substr(st,1,nchar(st)-1)), collapse="\n")
  cat(fin)
}  



#subset
short_iris <- iris[1:3,1:5]

# run function
df_2_MD(short_iris)
df_2_MD(iris)


