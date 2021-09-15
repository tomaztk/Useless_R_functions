##########################################
# 
# Annoying useless big digital clock
#
# Series:
# Little Useless-useful R functions #26
# Created: September 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


n0 <-
  c("██████"
    ,"██  ██"
    ,"██  ██"
    ,"██  ██"
    ,"██████")

n1 <-
  c("    ██"
    ,"    ██"
    ,"    ██"
    ,"    ██"
    ,"    ██")


n2 <-
  c("██████"
    ,"    ██"
    ,"██████"
    ,"██    "
    ,"██████")


n3 <-
  c("██████"
    ,"    ██"
    ,"██████"
    ,"    ██"
    ,"██████")


n4 <-
  c("██  ██"
    ,"██  ██"
    ,"██████"
    ,"    ██"
    ,"    ██")

n5 <-
  c("██████"
    ,"██    "
    ,"██████"
    ,"    ██"
    ,"██████")

n6 <-
  c("██████"
    ,"██    "
    ,"██████"
    ,"██  ██"
    ,"██████")

n7 <-
  c("██████"
    ,"    ██"
    ,"    ██"
    ,"    ██"
    ,"    ██")

n8 <-
  c("██████"
    ,"██  ██"
    ,"██████"
    ,"██  ██"
    ,"██████")

n9 <-
  c("██████"
    ,"██  ██"
    ,"██████"
    ,"    ██"
    ,"██████")


colon <-
  c("      "
    ,"  ██  "
    ,"      "
    ,"  ██  "
    ,"      ")


space <-
  c(" "
    ," "
    ," "
    ," "
    ," ")


df0 <- as.data.frame(n0)
df1 <- as.data.frame(n1)
df2 <- as.data.frame(n2)
df3 <- as.data.frame(n3)
df4 <- as.data.frame(n4)
df5 <- as.data.frame(n5)
df6 <- as.data.frame(n6)
df7 <- as.data.frame(n7)
df8 <- as.data.frame(n8)
df9 <- as.data.frame(n9)
dfc <- as.data.frame(colon)
dfs <- as.data.frame(space)

numbers <- cbind(df0, df1,df2,df3,df4,df5,df6,df7,df8,df9, dfc, dfs)

rm(df0, df1,df2,df3,df4,df5,df6,df7,df8,df9, dfc, dfs,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9, colon, space)


#print(numbers)

getVariable <- function(x = integer()){
  stopifnot(is.integer(x))
  
}


BigDitigalClock <- function() {
  
  while(TRUE){
    Sys.sleep(1)
    cat("\014")
    print.data.frame(numbers,  row.names = F)
    colnames(numbers) <- c(" "," "," "," "," "," "," "," "," "," "," "," ")
    
    #hour
    hh <- strftime(Sys.time(), format="%H")
    h1 <- substr(hh,1,1)
    h2 <- substr(hh,2,2)
    
    #minute
    mm <- strftime(Sys.time(), format="%M")
    m1 <- substr(mm,1,1)
    m2 <- substr(mm,2,2)
    
    #second
    ss <- strftime(Sys.time(), format="%S")
    s1 <- substr(ss,1,1)
    s2 <- substr(ss,2,2)
    
    
    
    #cat("\r", strftime(Sys.time(), format="%H:%M:%S"))
  }
}


#BigDitigalClock()




