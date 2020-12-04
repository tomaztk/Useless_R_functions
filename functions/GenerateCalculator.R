##########################################
# 
# Script that generates script for basic
# calculator functions for integers
# between 1 and 10
# Series:
# Little Useless-useful R functions #9
# Created: November 28, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
#        - adding x11()
###########################################

# Basic concept
calc <- function(a,b,oper){
  if(a==1 & b==1 & oper=="+"){print("Result is 2")}
  if(a==1 & b==1 & oper=="-"){print("Result is 0")}
  if(a==1 & b==1 & oper=="*"){print("Result is 1")}
  if(a==1 & b==1 & oper=="/"){print("Result is 1")}
  }

calc(1,1,"-")
calc(1,1,"+")
calc(1,1,"*")
calc(1,1,"/")

######################
##
## creating function
## to generate script
##
######################

# set all combinations
df <- data.frame(merge(merge(c(1:10), c(1:10), by=NULL), c("+","-","/","*"), by=NULL))
colnames(df) <- c("numberA", "numberB", "oper")
f <- "calc <- function(a,b,oper){"
for (i in 1:nrow(df)){
  res <- paste0(as.character(df$numberA[i]) , df$oper[i], as.character(df$numberB[i]))
  rr <- eval(parse(text=res))
  f1 <- paste0(' if(a==',as.character(df$numberA[i]), ' & b==', as.character(df$numberB[i]), ' & oper==', '"',as.character(df$oper[i]),'"' ,
               '){print("Result is ', as.character(rr),'")}', '\n' , collapse=NULL)
  f <<- paste0(f, f1, collapse = NULL)
  if(i==nrow(df)){
    f <<- paste0(f, "}", collapse = NULL)    
    eval(parse(text=f))
    }
}

calc(4,5,"/")



######################
##
## Alternative solution 
## (by mrdwab https://github.com/mrdwab)
## using expand.grid
## sprintf
## sapply
##
######################

df <- expand.grid(numA = 1:10, oper = c("+", "-", "/", "*"), numB = 1:10, stringsAsFactors = FALSE)
rr <- sapply(1:nrow(df), function(x) match.fun(df[x, "oper"])(df[x, "numA"], df[x, "numB"]))
template <- ' if (a == %s & b == %s & oper == "%s") print("Result is %g")\n '
f <- sprintf("calc <- function(a, b, oper) {\n%s\n}", 
             paste0(with(df, sprintf(template, numA, numB, oper, rr)), collapse = ""))
eval(parse(text = f))


