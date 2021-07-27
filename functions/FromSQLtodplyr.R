
############################
#
#
# Converts T-SQL to dplyr
#
#
#
#
############################

library(dplyr)
library(dbplyr)


## From dplyr to SQL
translate_sql(x == 1 && (y < 2 || z > 3))
translate_sql(cummean(G), vars_order = "year")
translate_sql(rank(), vars_group = "ID")

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
flights <- copy_to(con, nycflights13::flights)
airports <- copy_to(con, nycflights13::airports)



flights %>%
  select(contains("delay")) %>%
  show_query()


######################
## From  SQL to dplyr?
######################


q <- "SELECT * FROM flights"

toDplyr <- function(query){
  print(query)
  # parse query
    
}

toDplyr(q)

sample_query <- "SELECT `Sepal.Width`, `Species` FROM iris WHERE `Species` = `setosa` and `Petal.Length` >= 1.3 ORDER BY `Sepal.Width` ASC LIMIT 10"

dp <- "iris_subset <- iris %>%
          select(`Sepal.Width`, `Species`,`Petal.Length`) %>%
          filter(`Species` == \"setosa\" & `Petal.Length` >= 1.3 ) %>%
          arrange(desc(`Sepal.Width`)) %>%
          top_n(10)"

print(paste0("Dplyr query: ", dp))


eval(parse(text=dp))

iris_subset

rm(iris_subset)

######

q <- as.character ("SELECT * FROM iris WHERE species = 'setosa' AND  Petal.Lenght => 1.3")

insertToDataFrame <- function(q){
  
  # create df
  #qq <- data.frame(q=character(), q2 = character())
  
  # sapply the text
  qq <<- as.data.frame(sapply(strsplit(q, " "), function(x) print(x)))
  colnames(qq) <<- "qt"
  
  SQL_Reserved_words <- c("SELECT", "FROM","WHERE","IS","group by", "AS", "ORDER BY", "TOP", "OR", "ELSE", "CASE", "IN", "NULL", "NOT", "CASE",
                          "by", "having by", "LIKE", "OVER", "PERCENT", "when", "then", "convert", "cast", "distinct", "exists", "AND")
  
 match(SQL_Reserved_words, qq$qt)
  
}

insertToDataFrame(q)

#check data.frame
qq




