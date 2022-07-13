
######################
######################
##
##
## Eight R Tidyverse
## tips for better 
## data engineering
##
##
######################
######################


# Packages
library(tidyverse)
library(nycflights13)
# will be installed automatically if missing: ggplot2, purrr, tibble, dplyr, tidyr, stringr, readr , forcats

flights <- flights


#### -------------------------------------------------------
#  1. Use pipe "%>%" everywhere, including nesting functions
#### -------------------------------------------------------

# inner piping inside mutate
airlines %>%
  mutate(name_short = name %>%
           str_to_upper() %>%
           str_replace_all (" (INC|CO)\\.?$", "") %>%
           str_replace_all (" AIR ?(LINES|WAYS)?( CORPORATION)?$", "") %>%
           str_to_title() %>%
           str_replace_all("\\bUs\b", "US")
) %>%
  mutate(FullName_length = nchar(name)) %>%
  select (name_short, FullName_length) %>%
  arrange(desc(FullName_length))



# hard to read, of course
airlines %>% mutate(name = str_replace_all(
                                          str_to_title(
                                                      str_replace_all(
                                                                      str_replace_all(str_to_upper(name)," (INC|CO)\\.?$", ""),
                                                    " AIR ?(LINES|WAYS)?( CORPORATION)?$", "")),
                                          "\\bUs\b",
                              "US"))



#### -------------------------------------------------------
#  2. Operations across multiple columns at once
#### -------------------------------------------------------

# using single functions as ABS, Round, N_distinct
flights %>%
  mutate(across(c(dep_delay, arr_delay), abs))   

flights %>%
  mutate(across(c(dep_delay, arr_delay), round)) 

# adding where function to select columns
flights %>%
  summarise(across(where(is.integer), n_distinct))

# formatting
flights %>%
  mutate(across(c(dep_delay, arr_delay), as.integer))



# Using purr-style lambda functions!
flights %>%
  summarise(across(c(dep_time, arr_time), ~ sum(.x, na.rm = TRUE))) 

flights %>%
  summarise(across(c(dep_time, arr_time), ~ mean(.x, na.rm = TRUE)))

flights %>%
  summarise(across(where(is.integer), ~ sum(.x, na.rm = TRUE))) 


# using group_by
flights %>%
  group_by(origin) %>%
  summarise(across(c(dep_time, sched_dep_time, arr_time), n_distinct ))


flights %>%
  group_by(carrier) %>%
  summarise(across(c(dep_time,sched_arr_time,arr_time, sched_dep_time),  ~ sum(.x,  na.rm=TRUE)))


# using list of functions, defining column names and removing NA
flights %>%
  group_by(carrier) %>%
  summarise(across(ends_with("time"), list(AVG = mean, SD = sd, GrandTotal= ~ sum(is.na(.x))),  na.rm=TRUE, .names = "{.col}.{.fn}"))




#### -------------------------------------------------------
#  3. Case statement to create/change a column based on condition
#### -------------------------------------------------------

#simple case to give you an idea
x <- c(1,2,3,0,7,4,22,0,0,-1)

case_stat <- function(x){
  case_when(
    x == 0 ~ "NA",
    x >= 1 & x <= 4 ~ "From 1 to 4",
    x >= 5 & x <= 7 ~ "From 5 to 7",
    TRUE   ~ "Above 8 and beyond or negative :)"
  )
}
#run sample function
case_stat(x)


flights %>%
  group_by(carrier) %>%
  mutate(new_classification = case_when(
    (origin == "EWR") & (dep_delay <= 0) ~ "EWR with negative delay",
    (origin == "EWR") & (dep_delay > 0) ~ "EWR with positive delay",
    (origin == "JFK") ~ "Stats for JFK Airport",
    (origin == "LGA") &  (air_time <= 220) ~ "La Guardia under 6 hours flights",
    TRUE   ~ "La Guardia above 6 hours flights"
  )) %>%
  count(new_classification) %>%
  ungroup()



#### -------------------------------------------------------
#  4. Using transmute and comparison with mutate+select
#### -------------------------------------------------------
library(lubridate)

flights %>%
  mutate(date = make_date(year, month, day), carrier, tailnum) %>%
  select(date, carrier, tailnum)


flights %>%
  transmute(date = make_date(year, month, day), carrier, tailnum)



#### -------------------------------------------------------
# 5. Lumping levels for variable into factors and into "other"
#### -------------------------------------------------------

#sample
x <- factor(rep(LETTERS[1:15], times = c(20,15,23,2,4,3,1,1,1,5,2,8,3,1,1))) 
x %>% table()

x %>% fct_lump_n(5) %>% table()
x %>% fct_lump_min(5) %>% table()



flights %>%
  mutate(name = fct_lump_n(carrier, 5)) %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(x=name, y=n)) +
  geom_col() 



#### -------------------------------------------------------
#  6. Using crossing to generate all possible combinations
#### -------------------------------------------------------


crossing(
  age = c(30,40,50,60,70),
  status = c("New", "Used"),
  values = c("0-100EUR", "101-200E", "201-300", "301-400"),
  temperature = c(30,35,34)
)

# generating all possible combinations
flights %>% expand(origin, dest, dep_time,carrier)


# getting all possible combinations that are present in dataset using crossing
flights %>%
  select(origin, dest, dep_time,carrier) %>%
  crossing()
# > 97,946 more rows

# getting all possible combinations using expand and nesting
flights %>% expand(nesting(origin, dest, dep_time,carrier))
# > 97,946 more rows




#### -------------------------------------------------------
# 7. Reshaping data with pivot_wider and pivot_longer and spread/gather
#### -------------------------------------------------------

#pivot_wider

flights %>%
  group_by(carrier) %>%
  select(origin, dep_time, arr_time) %>%
  pivot_wider(
          names_from = origin, 
          values_from = c(dep_time,arr_time), 
          values_fn = ~mean(.x, na.rm = TRUE),
          names_glue = "{origin}_{.value}"
          )

#check calculation for carrier UA and origin EWR
flights %>%
  filter(carrier == 'UA' & origin == 'EWR') %>%
  group_by(carrier) %>%
    summarise(
    avg_dep_time = mean(dep_time, na.rm = TRUE)
  )


#pivot_longer

#create and persist dataframe called: flights_wider
flights_wider <- flights %>%
  group_by(carrier) %>%
  select(origin, dep_time, arr_time) %>%
  pivot_wider(
    names_from = origin, 
    values_from = c(dep_time,arr_time), 
    values_fn = ~mean(.x, na.rm = TRUE),
    names_glue = "{origin}_{.value}"
  )


flights_wider %>%
  pivot_longer(
    !carrier,
    names_to = "origin",
    values_to = "time",
    values_drop_na = TRUE
  )

#or with cols parameter and defining the pattern for column selection
flights_wider %>%
  pivot_longer(
    cols = ends_with("time"),
    names_to = "origin",
    values_to = "time",
    values_drop_na = TRUE
  )



#### -------------------------------------------------------
#  8. Adding ID to your dataframe
#### -------------------------------------------------------

# Associate ID with every row in dataset
flights %>% 
  mutate(running_id = row_number())

# or using tibble row_to_column function
flights2 <- tibble::rowid_to_column(flights, ID)


