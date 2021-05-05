#########################################
#
# Using SQL commands with R data-frame
#
########################################

val <- c(22,42,36,80,54)
name <- c("amber","ben","charles","daniel","eva")
lett <- LETTERS[21:25]

cdf <- data.frame (val, name, lett)

# get the values across three vectors
cdf[3,]



val2<- c(22,42,36,80,54,44,53,35,76,44,21)
name2 <- c("a2","b2","c2","d2","e2","f2","g2","h2","i2","j2","k2")
lett2 <- LETTERS[15:25]

cdf2 <- data.frame (val2, name2, lett2)


# get the values across three vectors
cdf2[3,]


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

cdf[6,]

iris <- iris

# using functions

sqldf("select [Sepal.Width] from iris
      where
        [Sepal.Width]  >= 3.0")

iris[iris$Sepal.Width >= 3.0,]$Sepal.Width

library(dplyr)

iris %>%
  select(Sepal.Width) %>%
  filter(Sepal.Width>=3.0)



#reshaping data
DF <- data.frame(g = rep(1:2, each = 5), t = rep(1:5, 2), v = 1:10)
t.names <- paste("t", unique(as.character(DF$t)), sep = "_")
a16r <- reshape(DF, direction = "wide", timevar = "t", idvar = "g", varying = list(t.names))

a16s <- sqldf("select 
	g, 
	sum((t == 1) * v) t_1, 
	sum((t == 2) * v) t_2, 
	sum((t == 3) * v) t_3, 
	sum((t == 4) * v) t_4, 
	sum((t == 5) * v) t_5 
from DF group by g")


sqldf("select count(*) as nof_rows
      FROM iris AS i1
      JOIN iris as i2
      ON i1.[Sepal.Width] = i2.[Sepal.Width] 
      WHERE
        i2.[Sepal.Width]  >= 3.0")



#inner,outer joins

sqldf("select * from cdf
      join cdf2 
      on cdf.val = cdf2.val2")


sqldf("select * from cdf
      WHERE
        val IN  (SELECT val2 FROM cdf2)")


sqldf("select * from cdf
      WHERE
        val not in (SELECT val2 FROM cdf2)")

sqldf("select * from cdf2
      WHERE
        val2 not in (SELECT val FROM cdf)")



# union / union all
sqldf("select val from cdf
      union 
      select val2 from cdf2")

# limit, order
sqldf("select val from cdf order by val DESC limit 1")



