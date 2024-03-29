##########################################
# 
# Annoying useless big digital clock
#
# Series:
# Little Useless-useful R functions #27
# Created: September 15, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


# Create Numbers
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

numbers <- cbind(df0, df1,df2,df3,df4,df5,df6,df7,df8,df9, dfc)
rm(df0, df1,df2,df3,df4,df5,df6,df7,df8,df9, dfc,n0,n1,n2,n3,n4,n5,n6,n7,n8,n9, colon)

# Get number / variable from data frame
getVariable <- function(x) {
  stopifnot(is.numeric(x))
  if (x == 0) {return (numbers$n0)}
  if (x == 1) {return (numbers$n1)}
  if (x == 2) {return (numbers$n2)}
  if (x == 3) {return (numbers$n3)}
  if (x == 4) {return (numbers$n4)}
  if (x == 5) {return (numbers$n5)}
  if (x == 6) {return (numbers$n6)}
  if (x == 7) {return (numbers$n7)}
  if (x == 8) {return (numbers$n8)}
  if (x == 9) {return (numbers$n9)}
}


BigDitigalClock <- function() {
  
  while(TRUE){
    Sys.sleep(1)
    cat("\014")
    
    #hour
    h1 <- substr(strftime(Sys.time(), format="%H"),1,1)
    h2 <- substr(strftime(Sys.time(), format="%H"),2,2)
    
    #minute
    m1 <- substr(strftime(Sys.time(), format="%M"),1,1)
    m2 <- substr(strftime(Sys.time(), format="%M"),2,2)
    
    #second
    s1 <- substr(strftime(Sys.time(), format="%S"),1,1)
    s2 <- substr(strftime(Sys.time(), format="%S"),2,2)
    
    dfh1 <- as.data.frame(getVariable(as.integer(h1)))
    dfh2 <- as.data.frame(getVariable(as.integer(h2)))
    dfm1 <- as.data.frame(getVariable(as.integer(m1)))
    dfm2 <- as.data.frame(getVariable(as.integer(m2)))
    dfs1 <- as.data.frame(getVariable(as.integer(s1)))
    dfs2 <- as.data.frame(getVariable(as.integer(s2)))
    
    current_time <- cbind(dfh1, dfh2, numbers$colon, 
                          dfm1, dfm2 , numbers$colon,
                          dfs1, dfs2)
    
    #Remove column namens and row names
    colnames(current_time) <- c(" "," "," "," "," "," "," "," ")
    print.data.frame(current_time,  row.names = F)
  }
}

# Run the clock
BigDitigalClock()
