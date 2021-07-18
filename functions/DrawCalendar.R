##########################################
# 
# Draw calendar for current month 
# Draw Years' complete calendar
#
# Series:
# Little Useless-useful R functions #28
# Created: July 17, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################


DrawCalendarMonth <- function(InDate=NULL) {
  if(is.null(InDate)){
    date <- Sys.Date()
  } else {
    date <- as.Date(InDate)
  }
  t <- format(date, "%m")
  TT <- format(date, "%B")
  year <- as.integer(format(date, "%Y"))
  
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        leap <- TRUE
      } else { leap <- FALSE }
    } else { leap <- TRUE }
  } else { leap <- FALSE }
  
  if (t %in% c("01","03","05", "07", "08", "10", "12")){
    nofDays <- 31
  } 
  if (t %in% c("04","06","09", "11")){
    nofDays <- 30
  }
  if (t %in% c("02") & leap == TRUE){
    nofDays <- 29
  } 
  if (t %in% c("02") & leap == FALSE){
    nofDays <- 28
  }
  
  firstDay <- as.Date(paste0(format(date, "%Y-%m"), "-01"))
  name1D <- weekdays(firstDay)
  num1D <- as.POSIXlt(firstDay)$wday
  n.col <- 7 #number of days; constant
  n.row <- 5 #depends on month, number of days and first day in month; must be calculated
  M_y <- 35  # Default matrix size (product of n.col * n.row)
  
  if (num1D > 5 & nofDays > 29) {
    n.row <- 6 #nrow correction
    M_y <- 42
  } 
  
  if (num1D %in% c(1) & nofDays %in% c(28)) {
    n.row <- 4 #nrow correction
    M_y <- 28
  } 
  
  mat <- matrix(1:M_y, nrow = n.row, ncol = n.col, T)
  
  #Reset values to 00
  for(column in 1:n.col){ mat[, column] <- 00 }
  i <- 1 #running value
  
  for(row in 1:n.row){
    for(column in 1:n.col){
      if(row >= 1 & column >= num1D | row > 1) {
        if(i > nofDays){
          mat[row,column] <- 0
          } else {
        mat[row,column] <- i
        i= i + 1
          }
      
      }
    }
  }
  dd <- as.data.frame(mat)
  colnames(dd) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  cat(paste0("\n", "Month: ",TT, "\n", "Year: ", year, "\n"))
 
  return(dd)
}


### Testing the calendar
DrawCalendarMonth() # OK
DrawCalendarMonth("2021-01-21") # OK
DrawCalendarMonth("2021-05-21") # OK 
DrawCalendarMonth("2021-02-21") # OK
DrawCalendarMonth("2020-02-21") # OK
DrawCalendarMonth("1992-08-29") # OK 



DrawYear <- function(Year=NULL){
  if(is.null(Year)){
    year <- as.integer(format(Sys.Date(), "%Y"))
  } else {
    year <- as.integer(Year)
  }
  dateFrom <- as.Date(paste0(year,"0101"), "%Y%m%d")
  dateTo <- as.Date(paste0(year,"1201"), "%Y%m%d")
  FirstOfMonth <- format(seq(dateFrom,dateTo,by="month"), "%Y-%m-%d")

  for (i in 1:length(FirstOfMonth)){
    r <- DrawCalendarMonth(FirstOfMonth[i])
    print(r)
    }
}

# Draw complete Calendar :-)
DrawYear(2020)
DrawYear()

