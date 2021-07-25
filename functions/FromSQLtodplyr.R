
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
