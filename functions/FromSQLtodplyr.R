
############################
#
#
# Converts T-SQL to dplyr
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



## From  SQL to dplyr?