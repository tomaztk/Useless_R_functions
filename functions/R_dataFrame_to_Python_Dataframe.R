##########################################
# 
# Export R Data.frame schema and data to  
# external file for Python Pandas
# 
# Series:
# Little Useless-useful R functions #15
# Created: January  04, 2021
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
# Changelog: 
#  
###########################################


## goal: Python Dictionary
## d = {'col1': [1, 2], 'col2': [3, 4]}


RtoPy <- function(df_input, filename_path) {
  
    # column names and number of rows
    Nn <- names(df_input)
    nr <- nrow(df_input)
    
#Python is Indentation  sensitive - leave these two lines without indentation   
py_df <- "import pandas as pd
d = {"
    
    for (x in 1:length(Nn)){
      var <- (Nn[x])
      #Column Names
      py_df <- paste0(py_df, "'",var,"':[", collapse=NULL)
      
      #Data Rows
      for (i in 1:nr) {
        val <- df_input[i,x]
        #Check for data type
        if (sapply(df_input[i,x], class) == "factor") {
          py_df <- paste0(py_df, "'",val,"'", ",", collapse=NULL)
          #for last value in a column
          if (i == nr){
            py_df <- paste0(py_df, "'",val,"'", "],","\n", collapse=NULL)
          } 
        } else {
          py_df <- paste0(py_df, val, ",", collapse=NULL)
          #for last value in a column
          if (i == nr){
            py_df <- paste0(py_df, val, "],","\n", collapse=NULL)
          }  
        }
        
      }
      if (x == length(Nn)){
        py_df <- substr(py_df, 1, nchar(py_df)-2)
        py_df <- paste0(py_df, "}
df=pd.DataFrame(data=d)", collapse=NULL)
      }
    }
    
    ## Store to file
    sink(file = filename_path)
    cat(py_df)
    sink(file = NULL)
}



# Get the data from R data.frame to Python Pandas script
iris <- iris
iris <- data.frame(iris[1:15,]) 
RtoPy(iris, "/users/tomazkastrun/desktop/iris_py.txt")
