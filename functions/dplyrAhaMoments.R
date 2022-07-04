#### Dplyr

library(dplyr)




# Case_when
x <- c(1,2,3,0,7,4,22,0,0)
case_stat <- function(x){
  
case_when(
  x == 0 ~ "NA",
  x >= 1 & x <= 4 ~ "Majke mi",
  x >= 5 & x <= 7 ~ "Yeah",
  TRUE   ~ "nikjer nič"
)

}

# slice
mtcars %>% slice()



c(factor("a"), factor(1))
c(factor("a"), factor("v"))
c(NULL)




############# ----------------------
## Tidyverse


library(tidyverse)
library(lubridate)
library(nycflights13)


# ------- tip1: Create new column in a count or group_by

flights %>%
  mutate(long_flight = (air_time >= 6*60)) %>%
  View()

flights %>%
  mutate(long_flight = (air_time >= 6*60)) %>%
  count(long_flight)

flights %>%
  count(long_flight = (air_time >= 6*60))

flights %>%
  count(flight_path = paste0(origin, " -> ", dest), sort = TRUE)

flights %>%
  group_by(datum_odhoda = make_date(year,month, day)) %>%
  summarise(flights_nof = n(),
            air_time_mediana = median(air_time, na.rm=TRUE)) %>%
  ungroup() %>%
#  mutate(total_avg = mean(air_time))

  
## Importance of ungroup

flights %>% 
    group_by(month) %>% 
    mutate(mean_AT = mean(air_time)) %>% 
    mutate(mean_D = mean(dep_delay)) %>% 
  ungroup()  %>%
  select(mean_AT, month, mean_D) 




# ------- tip2: Sample and randomly shuffle with  slice_sample()


flights %>%
  slice_sample(n=10)

flights %>%
  slice_sample(prop = 0.01)

flights %>%
  slice_sample(prop = 1)

flights %>%
  group_by(origin) %>%
  slice_sample (n=3)



# ------ tip3: making date time 

flights %>%
  select (day, month, year) %>%
  mutate(datum = make_date(year, month, day))


# ------ tip4: Parse numbers with parse_number()

num1 <- tibble(number_col = c("#1", "#2", "#3"))
num2 <- tibble(number_col = c("Number 3", "#4", "5"))
num3 <- tibble(number_col = c("1.2%", "32.3%", "3.123%")) 
num4 <- tibble(number_col = c("34cm", "123 cm", "3.4c", "43.41cm")) 
num5 <- tibble(number_col = c("12 EUR", "32.52 €", 3.4, "43 € 41 cents")) 


num1
num1 %>%
    mutate(num_col =  parse_number(number_col))

num2 %>%
  mutate(num_col =  parse_number(number_col))

num3 %>%
  mutate(num_col =  parse_number(number_col))

num4 %>%
  mutate(num_col =  parse_number(number_col))

num5 %>%
  mutate(num_col =  parse_number(number_col))


# ------ tip5: Select columns using starts_with and ends_with


flights %>%
  select(starts_with("dep_"))


flights %>%
  select(starts_with("dep_"), everything())

flights %>%
  select(ends_with("hour"))

flights %>%
  select(contains("dep"))


# ------ tip6: case_when to create (or change) a column based on conditions

flights %>%
  mutate(zacetek=case_when(
    origin == "EWR" ~ "Newark Airporto",
    origin == "JFK" ~ "Kenedijevo letalisce",
    origin == "LGA" &  air_time <= 220 ~ "La Guardia pod 6 ur",
    TRUE   ~ "La Guardia nad 6 ur"
  )) %>%
  count(zacetek)




# ------ tip7: str_replace_all to find and replace multiple options as once

flights %>%
  mutate (origin  = str_replace_all(origin, c(
    "^EWR$" = "New Arch Airoporto",
    "^JFK$" = "Janez F. K. letališče",
    "^LGA$" = "Letalisce GAS"
  ))) %>%
  count(origin)


# ------ tip8: transmute to create or change the column and keep only those columns
## difference between mute and transmute -> transmute keep only the columns created
flights %>%
  transmute(date = make_date(year, month, day), tailnum)

# ------ tip9: use pipe %>% everywhere including inside mutate

airlines %>%
    mutate(name = name %>%
             str_to_upper() %>%
             str_replace_all (" (INC|CO)\\.?$", "") %>%
             str_replace_all (" AIR ?(LINES|WAYS)?( CORPORATION)?$", "") %>%
             str_to_title() %>%
             str_replace_all("\\bUs\b", "US")
             )

# OR
# hard to read, of course
airlines %>% mutate(name = str_replace_all(str_to_title(str_replace_all(str_replace_all(str_to_upper(name),
                                            " (INC|CO)\\.?$", "")," AIR ?(LINES|WAYS)?( CORPORATION)?$", "")),"\\bUs\b", "US"))


# ------ tip10: Filter groups without making new column

flights %>%
  count(carrier, sort = TRUE)


flights_top_carriers <- flights %>%
  group_by (carrier) %>%
  filter(n() > 10000) %>%
  ungroup()

flights_top_carriers %>% 
  count(carrier, sort = TRUE)


# ------ tip11: Split the string into two columns based on regular expression
  
airlines %>%
  extract(
    name,
    into = c("short_name", "remainder"),
    regex = "^([^\\s]+) (.*)$"
  )

airlines %>%
  extract(
    name,
    into = c("short_name", "remainder"),
    regex = "^([^\\s]+) (.*)$",
    remove = FALSE
    )

# ------ tip12:  semi_join to pick only rows from the first table which are matched in the second table

airways_begining_with_a <- airlines %>%
  filter(name %>% str_detect("^A"))

flights %>%
  semi_join(airways_begining_with_a, by="carrier")

flights %>%
  semi_join(airways_begining_with_a, by="carrier") %>%
  count(carrier)


# ------ tip13:  anti_join  only select rows from the first table that are not (!) matched with second table

flights %>%
  anti_join (airways_begining_with_a, by = "carrier") 

flights %>%
  anti_join (airways_begining_with_a, by = "carrier") %>%
  count(carrier, sort = TRUE) %>%
  ungroup()

# ------ tip14: fct_reorder to sort bar chart

flights_with_airline_name <- flights %>%
  left_join(airlines, by = "carrier")

#unsorted
flights_with_airline_name %>%
  count(name) %>%
  ggplot(aes(name, n)) +
  geom_col()

# sorted
flights_with_airline_name %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col()

#----- tip15: coord_flip()

flights_with_airline_name %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col() + 
  coord_flip()


# --- tip16: using fct_lump to lump some factor levels into the "other"

flights_with_airline_name %>%
  mutate(name = fct_lump(name, n=5)) %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col() + 
  coord_flip()


# ------ tip17: use crossing to generate all possible combinations

crossing(
  starost = c(30,40,50,60,70),
  status = c("novi", "obstojeci"),
  placa = c("0-100EUR", "101-200E", "201-300", "301-400"),
  temperatura = c(30,35,34)
  
)

# ------ tip18: create function that take column names with double curly braces


col_summary <- function(data, col_names, na.rm = TRUE){
  data %>%
    summarise(across({{col_names}},
    list(
      min = min,
      max = max,
      median = median,
      mean = mean
    ),
  na.rm =na.rm,
  .names = "{col}_{fn}"
  ))
}

flights_with_airline_name %>%
  col_summary(c(air_time, arr_delay))

flights_with_airline_name %>%
  group_by(carrier) %>%
  col_summary(c(air_time, arr_delay))



# --- tip19: ggplot2, GGThemeAssist, esquise

library(ggplot2)
library(GGThemeAssist)
library(esquisss)


library(plotly)


#--- tip20: markdown, kableExtra





# -- tip21: bad vs. good loop

bad.loop <- function(n,x){
    result <- NULL
    for  (i in 1:n){
      result[i] <- x
    }
    result
}

bad.loop(19,33)

# Problem1: large number of iterations
# problem2: item result vector is reallocated and copied every run
# Problem3: GC must be triggered periodically
# Problem4: Many tiny computations over single iteration


better.loop <- function(n,x){
  result <- double(n)
  for  (i in 1:n){
    result[i] <- x
  }
  result
}

better.loop(19,33)

# improvement1: smaller allocation due to defining the vector (double(n))


vector.loop <- function(n,x){
  result <- double(n)
  result[] <- x
  result
}

vector.loop(19,33)

# improvement1: using vector assignement

built.in <- function(n,x){
  rep(x,n)
}

built.in(19,33)

library(microbenchmark)

n =1000000
x = 10
res <- microbenchmark(bad = bad.loop(n,x), okay = better.loop(n,x), vector = vector.loop(n,x), built_in_shit = built.in(n,x))

knitr::kable(summary(res, unit = "relative"))

autoplot(res)


# another bad loop
# problem 1. making and keeping copies (with rbind..)
bad.loop.norm <- function(n,x){
  m <- NULL
  for (i in 1:n){
    m <- rbind(m, rnorm(X))
  }
  m  
}

ok.norm <- function(n,x){
  m <- matrix(0, nrow=n, ncol=x)
  for (i in 1:n){
   m[i,] <- rnorm(100)
  }
  m  
}

faster.loop <- function(n,x){
  do.call('rbind', lapply(1:n, function(i) rnorm(x) ))
  }

