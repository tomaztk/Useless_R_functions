#
#
# Using SQL commands with R data-frame
#
#

val <- c(22,42,36,80,54)
name <- c("amber","ben","charles","daniel","eva")
lett <- LETTERS[21:25]

cdf <- data.frame (val, name, lett)

# get the values across three vectors
cdf[3,]

## What is table?

#install.packages("sqldf")
# load sqldf into workspace
library(sqldf)

# use SQL syntax to get the results from data.frame
sqldf("select * from cdf")
sqldf("select avg(val) AS avg_age from cdf") 

# merge innto single data.frame
new <- sqldf("select 10 as val,'Tom' as name,'Q' as lett") 

#sqldf("insert into cdf(val,name, lett)  values ('Tom',10,'Q')")
cdf <- sqldf(c("insert into cdf select * From new", "select * From cdf"))

cdf


#clean
rm(new,lett,name,val)

