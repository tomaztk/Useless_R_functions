#### Run useless API

library(plumber)
library(jsonlite)
library(swagger)

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 8000


cat("Command + c to exit")
cat(paste0(" Listening on  http://localhost:", port, "\n"))
cat(paste0(" Swagger UI    http://localhost:", port, "/__docs__/\n"))


pr <- plumb("plumber.R")

pr$run(
  host = "0.0.0.0",
  port = port,
  swagger = TRUE      # enables /__docs__/ interactive UI
)