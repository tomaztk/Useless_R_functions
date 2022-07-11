
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



# 2. Replacing/rounding/...  across multiple columns at once

# round
flights %>%
  mutate(across(c(dep_delay, arr_delay), abs))  #round)) 

flights %>%
  summarise(across(where(is.integer), n_distinct))


flights %>%
  summarise(across(where(is.integer), count ))




# formatting
flights %>%
  mutate(across(c(dep_delay, arr_delay), as.integer))


flights %>%
  mutate (origin  = str_replace_all(origin, c(
    "^EWR$" = "New Arch Airoporto",
    "^JFK$" = "Janez F. K. letališèe",
    "^LGA$" = "Letalisce GAS"
  ))) %>%
  count(origin)


flights %>%
  group_by(origin) %>%
  summarise(across(c(dep_time, sched_dep_time, arr_time)), n_distinct ) 



#flights %>%
#  group_by(carrier) %>%
#  summarise(across( c(dep_time,sched_arr_time,arr_time, sched_dep_time)),  ~ mean(.x,  na.rm=TRUE))


flights %>%
  group_by(carrier) %>%
  summarise(across(ends_with("time"), list(mean = mean, sd = sd), .names = "{.col}.{.fn}"))


flights %>%
  mutate(across(c(dep_time , sched_dep_time, arr_time, air_time)) ~ !is.na(.x))






# 3. Replacing values

# 4. Case_when to create (change) a column based on condition

flights %>%
  mutate(zacetek=case_when(
    origin == "EWR" ~ "Newark Airporto",
    origin == "JFK" ~ "Kenedijevo letalisce",
    origin == "LGA" &  air_time <= 220 ~ "La Guardia pod 6 ur",
    TRUE   ~ "La Guardia nad 6 ur"
  )) %>%
  count(zacetek)


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
