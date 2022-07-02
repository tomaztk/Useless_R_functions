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


# tip1: Create new column in a count or group_by

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
