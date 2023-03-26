#### create markdown table based on dataframe
## result: I want a markdown script with data of a dataframe
## e.g:
## 

# > iris[1:3,1:5]
# Result:
#> Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa

# and I want the result to be a markdown script, that can be used in Markdown document:

#> |Sepal.Length|Sepal.Width|Petal.Length|Petal.Width|Species|
#> |---|---|---|---|---|
#> |5.1|3.5|1.4|0.2|setosa|
#> |4.9|3|1.4|0.2|setosa|
#> |4.7|3.2|1.3|0.2|setosa|



your_df <- iris[1:3,1:5]

DF_2_MD <- function(your_df){
  
  cn <- as.character(names(your_df))
  headr <- paste0(c("", cn),  sep = "|", collapse='')
  sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), collapse=''),length(cn))), collapse ='')
  st <- "|"
    for (i in 1:nrow(your_df)){
      for(j in 1:ncol(your_df)){
        if (j%%ncol(your_df) == 0) {
          
          st <- paste0(st, your_df[i,j], "|", "\n", "" , "|", collapse = '')
        } else {
        st <- paste0(st, your_df[i,j], "|", collapse = '')
        }
      }
    }
  fin <- paste0(c(headr, sepr, st), collapse="\n")
  cat(fin)
}  

# run function
DF_2_MD(your_df)

