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


