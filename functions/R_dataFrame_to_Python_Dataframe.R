## Export R Data.frame to  R command ###Python Pandas


## goal: Python Dictionary
## d = {'col1': [1, 2], 'col2': [3, 4]}
## df = pd.DataFrame(data=d)
##

iris <- data.frame(iris[1:8,])
Nn <- names(iris)
nr <- nrow(iris)


sapply(iris[5], class)

py_df <- 
"import pandas as pd
d = {"

for (x in 1:length(Nn)){
    var <- (Nn[x])
    py_df <- paste0(py_df, "'",var,"':[", collapse=NULL)
      for (i in 1:nr) {
      #print(iris[i,x])
      val <- iris[i,x]
      if( apply(iris[5], class) == "factor" || sapply(iris[5], class) == "character")
      py_df <- paste0(py_df, val, ",", collapse=NULL)
      if (i == nr){
        py_df <- paste0(py_df, val, "],", collapse=NULL)
      }
      }
    if (x == length(Nn)){
      py_df <- paste0(py_df, "}
                      df=pd.DataFrame(data=d)", collapse=NULL)
  }
}

print(py_df)


