##########################################
# 
# Desk plant simulator in R
# R-Plant (Programmus enthusiasticus)
#
# Series:
# Little Useless-useful R functions #93
# Created: April 13, 2026
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.1

###########################################


#getwd()
#setwd("/Users/abcd/Desktop")
.plant_file <- path.expand("~/.r_desk_plant.rds")


# --  ASCII art for plant stages -- GPT Created
plant_art <- list(
  
  # Stage 0: Seed
  seed = c(
    "            ",
    "            ",
    "            ",
    "            ",
    "     .      ",
    "    (.)     ",
    "   -----    ",
    "  |     |   ",
    "  |~~~~~|   ",
    "  |_____|   "
  ),
  
  # Stage 1: Sprout
  sprout = c(
    "            ",
    "            ",
    "            ",
    "     ,      ",
    "    (')     ",
    "     |      ",
    "   -----    ",
    "  |     |   ",
    "  |~~~~~|   ",
    "  |_____|   "
  ),
  
  # Stage 2: Seedling
  seedling = c(
    "            ",
    "            ",
    "     \\|/    ",
    "    \\|||/   ",
    "     |||    ",
    "     |||    ",
    "   -----    ",
    "  |     |   ",
    "  |~~~~~|   ",
    "  |_____|   "
  ),
  
  # Stage 3: Young plant
  young = c(
    "            ",
    "    \\~~/    ",
    "   \\\\|//   ",
    "    \\|/     ",
    "    |||     ",
    "    |||     ",
    "   -----    ",
    "  |     |   ",
    "  |~~~~~|   ",
    "  |_____|   "
  ),
  
  # Stage 4: Growing
  growing = c(
    "    _\\_     ",
    "   / o \\   ",
    "   \\~~~/ /  ",
    "    ||| /   ",
    "    |||/    ",
    "    |||     ",
    "   -----    ",
    "  |     |   ",
    "  |~~~~~|   ",
    "  |_____|   "
  ),
  
  # Stage 5: Mature
  mature = c(
    "   \\    /   ",
    "    )  (    ",
    "   / @@ \\  ",
    "  ( @  @ )  ",
    "   \\~~~~/ / ",
    "    ||||/   ",
    "    ||||    ",
    "   ------   ",
    "  |~~~~~~|  ",
    "  |______|  "
  ),
  
  # Stage 6: Flowering
  flowering = c(
    "    \\***/   ",
    "   * @ @ *  ",
    "  * @@@@@ * ",
    "  ( @ @ @ ) ",
    "   \\~~~~/ / ",
    "    ||||/   ",
    "    ||||    ",
    "   ------   ",
    "  |~~~~~~|  ",
    "  |______|  "
  ),
  
  # Stage 7: Full bloom / Tree
  tree = c(
    "    (@@)    ",
    "  ((@@@@@)) ",
    " ((@@*@@@@))",
    " ((@@@*@@@))",
    "  ((@@@@@@))",
    "    ||||    ",
    "    ||||    ",
    "   ------   ",
    "  |~~~~~~|  ",
    "  |______|  "
  ),
  
  # Wilted (unhealthy)
  wilted = c(
    "            ",
    "            ",
    "      _     ",
    "     / )    ",
    "    | /     ",
    "    |/      ",
    "   -----    ",
    "  |     |   ",
    "  |.....|   ",
    "  |_____|   "
  ),
  
  # Dead
  dead = c(
    "            ",
    "            ",
    "            ",
    "    ---     ",
    "   /   \\   ",
    "   \\RIP/    ",
    "   -----    ",
    "  |     |   ",
    "  |.....|   ",
    "  |_____|   "
  )
)


# --- grwoth stages - created with use of GPT!!!!

growth_stages <- data.frame(
  stage = 0:7,
  name = c("Seed", "Sprout", "Seedling", "Young Plant", 
           "Growing", "Mature", "Flowering", "Full Bloom"),
  art_name = c("seed", "sprout", "seedling", "young", 
               "growing", "mature", "flowering", "tree"),
  min_points = c(0, 10, 25, 50, 100, 175, 275, 400),
  emoji = c("🫘", " ", "🌿", "☘️", "🪴", "🌳", "🌸", "🌲"),
  stringsAsFactors = FALSE
)


# --- helper functions

hline <- function(char = "═", n = 40) strrep(char, n)

# get current status of a plant
get_plant <- function(file = .plant_file) {
  if (file.exists(file)) {
    readRDS(file)
  } else {
    NULL
  }
}


# Save plant in RDS and all game play
save_plant <- function(plant, file = .plant_file) {
  saveRDS(plant, file)
}


# get growth info
get_stage <- function(points) {
  stage_idx <- max(which(growth_stages$min_points <= points))
  growth_stages[stage_idx, ]
}


# get appropriate ASCII art plant
get_plant_art <- function(plant) {
  if (plant$health <= 0) {
    return(plant_art$dead)
  }
  if (plant$health < 30) {
    return(plant_art$wilted)
  }
  stage <- get_stage(plant$points)
  plant_art[[stage$art_name]]
}


# days calculation
days_since <- function(date) {
  as.integer(Sys.Date() - as.Date(date))
}


# Messages for encouragement
get_encouragement <- function() {
  messages <- c(
    "Your plant appreciates you!",
    "Keep up the great work!",
    "You're a natural plant parent!",
    "Your R sessions make the plant happy!",
    "Photosynthesis in progress... ",
    "Growing strong, just like your R skills!",
    "The plant sends positive vibes!",
    "Another day, another leaf!",
    "Your dedication is blooming!",
    "Keep coding, keep growing!"
  )
  sample(messages, 1)
}


# General health
get_health_message <- function(health) {
  if (health >= 90) {
    return("Thriving! Your plant is radiantly healthy!")
  } else if (health >= 70) {
    return("Healthy! Looking good!")
  } else if (health >= 50) {
    return("Okay. Could use some attention.")
  } else if (health >= 30) {
    return("Struggling. Please water me!")
  } else if (health > 0) {
    return("Critical! Water immediately or I'll die!")
  } else {
    return("Your plant has died. Start a new one with PlantNew()")
  }
}


# ---- main functions

PlantNew <- function(name = NULL, file = .plant_file) {
  
  # Check if plant already exists
  existing <- get_plant(file)
  
  if (!is.null(existing) && existing$health > 0) {
    cat("  You already have a plant named '", existing$name, "'!\n", sep = "")
    cat(" Health: ", existing$health, "% | Stage: ", 
        get_stage(existing$points)$name, "\n\n", sep = "")
    cat("  Are you sure you want to replace it? (yes/no): ")
    response <- tolower(readline())
    if (response != "yes") {
      cat("  Keeping your existing plant.  \n\n")
      return(NULL)
    }
  }
  
  
  # Get plant name
  if (is.null(name)) {
    suggestions <- c("Fernie Bennes", "Morgan Treeman", "Leaf Seinfeld",
                     "Plantonio Banderas", "Elvis Parsley", "Kramerofern",
                     "Snake Costanza", "Aloe NewmanVera", "Jungle Tribbiani")
    
    cat("\n")
    cat("    NEW PLANT!\n")
    cat("  Some name suggestions:\n")
    for (i in seq_along(suggestions)) {
      cat(sprintf("    %d. %s\n", i, suggestions[i]))
    }
    cat("\n  Enter a name (or number, or press Enter for random): ")
    input <- readline()
    
    if (input == "") {
      name <- sample(suggestions, 1)
    } else if (grepl("^[0-9]+$", input)) {
      idx <- as.integer(input)
      if (idx >= 1 && idx <= length(suggestions)) {
        name <- suggestions[idx]
      } else {
        name <- input
      }
    } else {
      name <- input
    }
  }
  
  # Create new plant
  plant <- list(
    name = name,
    species = "R-Plant (Programmus enthusiasticus)",
    planted_date = Sys.Date(),
    last_watered = Sys.Date(),
    last_visited = Sys.time(),
    points = 0,
    health = 100,
    times_watered = 0,
    sessions = 0,
    achievements = character(0)
  )
  
  save_plant(plant, file)
  
  # Display
  cat("\n")
  cat("  ╔", hline("═", 44), "╗\n", sep = "")
  cat("  ║         NEW PLANT CREATED!               ║\n")
  cat("  ╚", hline("═", 44), "╝\n", sep = "")
  cat("\n")
  
  # Show seed art
  art <- plant_art$seed
  for (line in art) {
    cat("        ", line, "\n", sep = "")
  }
  
  cat("\n")
  cat("  Name:    ", name, "\n", sep = "")
  cat("  Species: ", plant$species, "\n", sep = "")
  cat("  Planted: ", format(Sys.Date(), "%B %d, %Y"), "\n", sep = "")
  cat("\n")
  cat("  💡 Tips:\n")
  cat("     • Use WaterPlant() to water your plant\n")
  cat("     • Use CheckPlant() to see its status\n")
  cat("     • Visit often - your R sessions help it grow!\n")
  cat("\n")
  
  invisible(plant)
}



# -- Water planting 
WaterPlant <- function(file = .plant_file) {
  
  plant <- get_plant(file)
  
  if (is.null(plant)) {
    cat("\n No plant found! Start one with PlantNew()\n\n")
    return(NULL)
  }
  
  if (plant$health <= 0) {
    cat("\n  Your plant has died. Start a new one with PlantNew()\n\n")
    return(NULL)
  }
  
  # Check if already watered today
  last_water_date <- as.Date(plant$last_watered)
  today <- Sys.Date()
  
  if (last_water_date == today) {
    cat("\n")
    cat("  Already watered today!\n")
    cat("  Your plant doesn't want to drown. \n")
    cat("  Come back tomorrow!\n\n")
    return(invisible(plant))
  }
  
  # Calculate bonus for consecutive days
  days_since_water <- days_since(plant$last_watered)
  
  # Water the plant
  water_points <- 15
  health_gain <- 20
  
  # Bonus for not over-neglecting
  if (days_since_water == 1) {
    water_points <- water_points + 5  # Consecutive day bonus
    plant$achievements <- union(plant$achievements, "daily_waterer")
  }
  
  plant$last_watered <- today
  plant$points <- plant$points + water_points
  plant$health <- min(100, plant$health + health_gain)
  plant$times_watered <- plant$times_watered + 1
  plant$last_visited <- Sys.time()
  
  # Check for achievements
  if (plant$times_watered == 10 && !"10_waters" %in% plant$achievements) {
    plant$achievements <- c(plant$achievements, "10_waters")
  }
  if (plant$times_watered == 50 && !"50_waters" %in% plant$achievements) {
    plant$achievements <- c(plant$achievements, "50_waters")
  }
  
  save_plant(plant, file)
  
  # Get stage info
  old_stage <- get_stage(plant$points - water_points)
  new_stage <- get_stage(plant$points)
  leveled_up <- new_stage$stage > old_stage$stage
  
  # Display
  cat("\n")
  cat("  ╔", hline("═", 44), "╗\n", sep = "")
  cat("  ║            WATERING TIME!                 ║\n")
  cat("  ╚", hline("═", 44), "╝\n", sep = "")
  cat("\n")
  
  # Animation-like effect
  cat("    Watering '", plant$name, "'...\n\n", sep = "")
  
  # Show art
  art <- get_plant_art(plant)
  for (line in art) {
    cat("        ", line, "\n", sep = "")
  }
  
  cat("\n")
  cat(sprintf("  +%d growth points! (Total: %d)\n", water_points, plant$points))
  cat(sprintf("  Health: %d%% %s\n", plant$health, 
              strrep("█", plant$health %/% 10)))
  
  # Level up message
  if (leveled_up) {
    cat("\n")
    cat("  ╔", hline("═", 44), "╗\n", sep = "")
    cat("     LEVEL UP! Your plant is now: ", new_stage$emoji, " ", new_stage$name, "\n", sep = "")
    cat("  ╔", hline("═", 44), "╗\n", sep = "")
  }
  
  cat("\n  ", get_encouragement(), "\n\n", sep = "")
  
  invisible(plant)
}


# --  Check on your desk plant
CheckPlant <- function(file = .plant_file) {
  
  plant <- get_plant(file)
  
  if (is.null(plant)) {
    cat("\n    No plant found! Start one with PlantNew()\n\n")
    return(invisible(NULL))
  }
  
  # Calculate health decay based on days since last water
  days_without_water <- days_since(plant$last_watered)
  
  if (days_without_water > 1 && plant$health > 0) {
    # Lose health for each day without water (after first day)
    health_loss <- (days_without_water - 1) * 10
    plant$health <- max(0, plant$health - health_loss)
  }
  
  # Award visit points (small amount, encourages checking in)
  # But only once per R session (track with session time)
  session_start <- Sys.getenv("R_SESSION_TMPDIR")  # Unique per session
  
  visit_points <- 2
  plant$points <- plant$points + visit_points
  plant$sessions <- plant$sessions + 1
  plant$last_visited <- Sys.time()
  
  # Check for session achievements
  if (plant$sessions == 100 && !"100_sessions" %in% plant$achievements) {
    plant$achievements <- c(plant$achievements, "100_sessions")
  }
  
  save_plant(plant, file)
  
  # Get stage info
  stage <- get_stage(plant$points)
  days_alive <- days_since(plant$planted_date)
  
  # Display
  cat("\n")
  cat("  ╔", hline("═", 44), "╗\n", sep = "")
  cat("  ║          PLANT STATUS REPORT              ║\n")
  cat("  ╚", hline("═", 44), "╝\n", sep = "")
  cat("\n")
  
  # Show art
  art <- get_plant_art(plant)
  for (line in art) {
    cat("        ", line, "\n", sep = "")
  }
  
  cat("\n")
  cat("  ", hline("─", 44), "\n", sep = "")
  cat(sprintf("  Name:        %s\n", plant$name))
  cat(sprintf("  Age:         %d day%s old\n", days_alive, if(days_alive != 1) "s" else ""))
  cat(sprintf("  Stage:       %s %s\n", stage$emoji, stage$name))
  cat(sprintf("  Points:      %d / %d (next stage)\n", 
              plant$points, 
              ifelse(stage$stage < 7, growth_stages$min_points[stage$stage + 2], "MAX")))
  cat("  ", hline("─", 44), "\n", sep = "")
  
  # Health bar
  health_bar <- paste0(
    strrep("█", plant$health %/% 10),
    strrep("░", 10 - plant$health %/% 10)
  )
  cat(sprintf("  Health:      [%s] %d%%\n", health_bar, plant$health))
  cat(sprintf("  Status:      %s\n", get_health_message(plant$health)))
  cat("  ", hline("─", 44), "\n", sep = "")
  
  # Water status
  days_since_water <- days_since(plant$last_watered)
  if (days_since_water == 0) {
    water_status <- " Watered today!"
  } else if (days_since_water == 1) {
    water_status <- " Watered yesterday"
  } else {
    water_status <- sprintf("   %d days without water!", days_since_water)
  }
  cat(sprintf("  Last water:  %s\n", water_status))
  
  cat("  ", hline("─", 44), "\n", sep = "")
  
  # Stats
  cat(sprintf("  Waterings:   %d total\n", plant$times_watered))
  cat(sprintf("  Visits:      %d R sessions\n", plant$sessions))
  
  # Achievements
  if (length(plant$achievements) > 0) {
    cat("  ", hline("─", 44), "\n", sep = "")
    cat("    Achievements:\n")
    achievement_names <- list(
      "daily_waterer" = "Daily Waterer - Watered on consecutive days",
      "10_waters" = "Hydration Helper - Watered 10 times",
      "50_waters" = "Water Master - Watered 50 times",
      "100_sessions" = "Dedicated Parent - 100 R sessions"
    )
    for (ach in plant$achievements) {
      if (ach %in% names(achievement_names)) {
        cat(sprintf("  %s\n", achievement_names[[ach]]))
      }
    }
  }
  
  cat("\n")
  
  # Reminder to water
  if (days_since_water >= 1 && plant$health > 0) {
    cat("  Don't forget to WaterPlant()!\n\n")
  }
  
  invisible(plant)
}


# -- View your plant's growth history
PlantJourney <- function(file = .plant_file) {
  
  plant <- get_plant(file)
  
  if (is.null(plant)) {
    cat("\n    No plant found! Start one with PlantNew()\n\n")
    return(invisible(NULL))
  }
  
  current_stage <- get_stage(plant$points)
  
  cat("\n")
  cat("  ╔", hline("═", 48), "╗\n", sep = "")
  cat("  ║             GROWTH JOURNEY                    ║\n")
  cat("  ║             '", sprintf("%-20s", plant$name), "'        ║\n", sep = "")
  cat("  ╚", hline("═", 48), "╝\n", sep = "")
  cat("\n")
  
  # Show progress through stages
  for (i in 1:nrow(growth_stages)) {
    row <- growth_stages[i, ]
    
    if (row$stage <= current_stage$stage) {
      # Reached this stage
      status <- "✓"
      marker <- row$emoji
    } else {
      # Not yet reached
      status <- " "
      marker <- "⬜"
    }
    
    # Progress bar to next stage
    if (row$stage == current_stage$stage && row$stage < 7) {
      next_threshold <- growth_stages$min_points[i + 1]
      current_threshold <- row$min_points
      progress <- (plant$points - current_threshold) / (next_threshold - current_threshold)
      bar <- paste0(
        strrep("█", floor(progress * 10)),
        strrep("░", 10 - floor(progress * 10))
      )
      cat(sprintf("  %s %s %-12s [%s] %d/%d pts\n",
                  status, marker, row$name, bar, plant$points, next_threshold))
    } else {
      if (row$stage < current_stage$stage) {
        cat(sprintf("  %s %s %-12s [██████████] Complete!\n",
                    status, marker, row$name))
      } else if (row$stage == current_stage$stage && row$stage == 7) {
        cat(sprintf("  %s %s %-12s [██████████] MAX LEVEL! \n",
                    status, marker, row$name))
      } else {
        cat(sprintf("  %s %s %-12s [░░░░░░░░░░] %d pts needed\n",
                    status, marker, row$name, row$min_points))
      }
    }
  }
  
  cat("\n")
  cat(sprintf("  Total points: %d\n", plant$points))
  cat(sprintf("  Days growing: %d\n", days_since(plant$planted_date)))
  cat("\n")
  
  invisible(plant)
}


# -- Display plant gallery showing all growth stages
PlantGallery <- function() {
  
  cat("\n")
  cat("  ╔", hline("═", 52), "╗\n", sep = "")
  cat("  ║           ️   PLANT GROWTH GALLERY️               ║\n")
  cat("  ╚", hline("═", 52), "╝\n", sep = "")
  cat("\n")
  
  stages_to_show <- c("seed", "sprout", "seedling", "young", 
                      "growing", "mature", "flowering", "tree")
  stage_names <- c("Seed (0 pts)", "Sprout (10 pts)", "Seedling (25 pts)", 
                   "Young (50 pts)", "Growing (100 pts)", "Mature (175 pts)",
                   "Flowering (275 pts)", "Full Bloom (400 pts)")
  
  # Show two per row
  for (i in seq(1, length(stages_to_show), by = 2)) {
    art1 <- plant_art[[stages_to_show[i]]]
    art2 <- if (i + 1 <= length(stages_to_show)) plant_art[[stages_to_show[i + 1]]] else rep("", 10)
    
    # Print header
    cat(sprintf("  %-24s  %-24s\n", stage_names[i], 
                if (i + 1 <= length(stages_to_show)) stage_names[i + 1] else ""))
    cat("  ", hline("─", 24), "  ", hline("─", 24), "\n", sep = "")
    
    # Print art side by side
    for (j in seq_along(art1)) {
      line1 <- if (j <= length(art1)) art1[j] else ""
      line2 <- if (j <= length(art2)) art2[j] else ""
      cat(sprintf("  %-24s  %-24s\n", line1, line2))
    }
    cat("\n")
  }
  
  # Show special states
  cat("     Special States:\n")
  cat("  ", hline("─", 24), "  ", hline("─", 24), "\n", sep = "")
  cat(sprintf("  %-24s  %-24s\n", "Wilted (health < 30%)", "Dead (health = 0%)"))
  cat("  ", hline("─", 24), "  ", hline("─", 24), "\n", sep = "")
  
  art_wilted <- plant_art$wilted
  art_dead <- plant_art$dead
  
  for (j in seq_along(art_wilted)) {
    cat(sprintf("  %-24s  %-24s\n", art_wilted[j], art_dead[j]))
  }
}


# Quick status - just show the plant
Plant <- function(file = .plant_file) {
  plant <- get_plant(file)
  
  if (is.null(plant)) {
    cat("\n No plant! Use PlantNew() to start.\n\n")
    return(NULL)
  }
  
  # Update health decay silently
  days_without_water <- days_since(plant$last_watered)
  if (days_without_water > 1 && plant$health > 0) {
    health_loss <- (days_without_water - 1) * 10
    plant$health <- max(0, plant$health - health_loss)
    save_plant(plant, file)
  }
  
  stage <- get_stage(plant$points)
  art <- get_plant_art(plant)
  
  cat("\n")
  for (line in art) {
    cat("        ", line, "\n", sep = "")
  }
  cat("\n")
  cat(sprintf("  %s %s | Day %d | %d%% health\n",
              stage$emoji, plant$name, 
              days_since(plant$planted_date),
              plant$health))
  cat("\n")
}


PlantDelete <- function(file = .plant_file, confirm = TRUE) {
  
  plant <- get_plant(file)
  
  if (is.null(plant)) {
    cat("\n  No plant to delete.\n\n")
    return(invisible(FALSE))
  }
  
  if (confirm) {
    cat("\n  Are you sure you want to delete '", plant$name, "'?\n", sep = "")
    cat("  This cannot be undone! (yes/no): ")
    response <- tolower(readline())
    if (response != "yes") {
      cat("  Keeping your plant safe.  \n\n")
      return(invisible(FALSE))
    }
  }
  
  file.remove(file)
  cat("\n Goodbye, ", plant$name, "...\n", sep = "")
  cat(" Use PlantNew() to start fresh.\n\n")
}

#optional
.onAttach <- function() {
  plant <- get_plant()
  if (!is.null(plant) && plant$health > 0) {
    stage <- get_stage(plant$points)
    message(sprintf(" Your plant '%s' (%s) is waiting! Use CheckPlant() to visit.",
                    plant$name, stage$name))
  }
}

# Run welcome check
local({
  plant <- get_plant()
  if (!is.null(plant) && plant$health > 0) {
    days_without_water <- days_since(plant$last_watered)
    stage <- get_stage(plant$points)
    cat(sprintf("\n '%s' says hello! (%s, Day %d)\n",
                plant$name, stage$name, days_since(plant$planted_date)))
    if (days_without_water >= 2) {
      cat(sprintf("It's been %d days without water!\n", days_without_water))
    }
    cat("\n")
  }
})


### Usage

PlantNew()
WaterPlant()   
CheckPlant()     
Plant()      
PlantJourney()  
PlantGallery()  
PlantDelete()    

 

