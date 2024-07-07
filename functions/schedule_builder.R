rm(list = ls())

library(combinat)
departments <- c("DEP1", "DEP2", "DEP3", "DEP4", "DEP5", "DEP6")
time_slots <- 5
rooms <- 3

department_pairs <- combn(departments, 2, simplify = FALSE)

set.seed(124)  # 124 je ok; 123 ni ok
department_pairs <- sample(department_pairs)

timetable <- matrix(list(), nrow = time_slots, ncol = rooms)

can_schedule <- function(pair, slot, room, timetable) {
  d1 <- pair[1]
  d2 <- pair[2]
  for (r in 1:rooms) {
    if (length(timetable[[slot, r]]) > 0 && (d1 %in% timetable[[slot, r]] || d2 %in% timetable[[slot, r]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}

fill_timetable <- function(timetable, department_pairs) {
  used_pairs <- rep(FALSE, length(department_pairs))
  
  for (slot in 1:time_slots) {
    for (room in 1:rooms) {
      for (pair_index in 1:length(department_pairs)) {
        pair <- department_pairs[[pair_index]]
        if (!used_pairs[pair_index] && can_schedule(pair, slot, room, timetable)) {
          timetable[[slot, room]] <- pair
          used_pairs[pair_index] <- TRUE
          break
        }
      }
    }
  }
  return(timetable)
}


timetable <- fill_timetable(timetable, department_pairs)

is_complete <- all(sapply(timetable, length) > 0)

if (is_complete) {
  schedule <- matrix(NA, nrow = time_slots, ncol = rooms)
  for (slot in 1:time_slots) {
    for (room in 1:rooms) {
      if (length(timetable[[slot, room]]) > 0) {
        schedule[slot, room] <- paste(timetable[[slot, room]][1], timetable[[slot, room]][2], sep = ":")
      }
    }
  }
  #fancy stuff
  schedule <- as.data.frame(schedule)
  colnames(schedule) <- paste("Room", 1:rooms, sep = " ")
  rownames(schedule) <- paste("Time Slot", 1:time_slots, sep = " ")
  print(schedule)
  
} else {
  print("generate new seed to get schedule")
  print(as.data.frame(timetable))
  
}


