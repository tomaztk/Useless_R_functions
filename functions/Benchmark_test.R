#useless benchmarking


# install.packages("rbenchmark")
library(rbenchmark)

rows <- 10^4
hex <- c(0:9, LETTERS[1:6])

set.seed(2022)
dt <- data.frame("red" = sample(0:255, rows, replace = TRUE), 
                 "green" = sample(0:255, rows, replace = TRUE),
                 "blue" = sample(0:255, rows, replace = TRUE))

rbenchmark::benchmark(
  "for loop" = {
    df <- dt
    for (r in 1:nrow(df)) {
      df$hexFor[r] <- paste0("#", 
                             hex[floor(df$red[r] / 16) + 1],
                             hex[df$red[r] %% 16 + 1],
                             hex[floor(df$green[r] / 16) + 1],
                             hex[df$green[r] %% 16 + 1],
                             hex[floor(df$blue[r] / 16) + 1],
                             hex[df$blue[r] %% 16 + 1]
      )
    }
  },
  "apply" = {
    df <- dt
    rgbToHex <- function(x) {
      paste0("#",
             hex[floor(x["red"] / 16) + 1],
             hex[x["red"] %% 16 + 1],
             hex[floor(x["green"] / 16) + 1],
             hex[x["green"] %% 16 + 1],
             hex[floor(x["blue"] / 16) + 1],
             hex[x["blue"] %% 16 + 1]
      )
    }
    df$hexApply <- apply(df, 1, rgbToHex)
  },
  "vector" = {
    df <- dt
    df$hexVector <- paste0("#",
                           hex[floor(df$red / 16) + 1],
                           hex[df$red %% 16 + 1],
                           hex[floor(df$green / 16) + 1],
                           hex[df$green %% 16 + 1],
                           hex[floor(df$blue / 16) + 1],
                           hex[df$blue %% 16 + 1]
    )
  },
  replications = 10, order = "relative"
)