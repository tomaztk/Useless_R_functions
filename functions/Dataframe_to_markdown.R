#### create markdown table based on dataframe
## result: I want a markdown script with data of a dataframe
## e.g:
## 

# > iris[1:2,1:3]
# Result

# Sepal.Length Sepal.Width Petal.Length
# 1          5.1         3.5          1.4
# 2          4.9         3.0          1.4

# and I want the result to be a markdown script, that can be used in Markdown document:

# |Sepal.Length |Sepal.Width | Petal.Length |
#   |---|---|---|
#   |5.1         |3.5          |1.4|
#   |4.9         |3.0          |1.4|



your_df <- iris[1:2,1:3]


DF_2_MD <- function(your_df){

   cn <- as.character(names(your_df))
   

    headr <- paste0(c("", cn),  sep = "|")
    sepr <- paste0(c(rep(paste0(c("|",paste0(rep("-", 3) , sep = "" ))), length(cn)), "|"))
    sepr <- sub(' ','_', sepr)
    st <- "|"
    for (i in 1:nrow(your_df)){
      for(j in 1:ncol(your_df)){
        if (i%%ncol(your_df) == 0) {
          st <- paste0(st, your_df[i,j], "|", "\n" , "|")
        } else {
        st <- paste0(st, your_df[i,j], "|")
        }
      }
    }
    
     fin <- paste(c(headr, sepr, st), sep="\n")
    #fin <- toString(paste0(c(headr, '\n', sepr, '\n', st)), collapse="")
  cat(fin)
}  


# run function
DF_2_MD(your_df)
