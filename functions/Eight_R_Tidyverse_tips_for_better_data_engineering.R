
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




# 5. Lumping some variables into factors and others into "other"

# 6. Using crossing to generate all possible combinations

crossing(
  starost = c(30,40,50,60,70),
  status = c("novi", "obstojeci"),
  placa = c("0-100EUR", "101-200E", "201-300", "301-400"),
  temperatura = c(30,35,34)
)



# 7. Reshaping data with pivot_wider and pivot_longer and spread/gather



# 8. Importing data into specified column types


# 9. Adding ID to your dataframe
flights %>%
  mutate(running_id = row_number())



flights %>%
  mutate (origin  = str_replace_all(origin, c(
    "^EWR$" = "New Arch Airoporto",
    "^JFK$" = "Janez F. K. letališèe",
    "^LGA$" = "Letalisce GAS"
  ))) %>%
  count(origin)