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
