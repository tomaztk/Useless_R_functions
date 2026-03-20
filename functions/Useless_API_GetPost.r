########################
### useless iris API
#######################

# install.packages("plumber")
library(plumber)
library(swagger)
library(webutils)
library(jsonlite)


iris_data <- iris
colnames(iris_data) <- tolower(gsub("\\.", "_", colnames(iris_data)))
# sepal_length | sepal_width | petal_length | petal_width | species



function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 204
    return(list())
  }
  plumber::forward()
}


#* @get /
#* @serializer json
function() {
  list(
    message  = "Welcome to the Iris Dataset API",
    version  = "1.0.0",
    endpoints = list(
      GET  = list(
        "/"              = "This help message",
        "/iris"          = "All rows (supports ?species=, ?min_sepal_length=, ?max_sepal_length=, ?limit=)",
        "/iris/species"  = "List of unique species",
        "/iris/summary"  = "Summary statistics per species",
        "/iris/{id}"     = "Single row by 1-based row index"
      ),
      POST = list(
        "/iris" = "Insert a new row. Body: {sepal_length, sepal_width, petal_length, petal_width, species}"
      )
    )
  )
}


#* @get /iris
#* @param species      Filter by species name (setosa | versicolor | virginica)
#* @param min_sepal_length Minimum sepal length (numeric)
#* @param max_sepal_length Maximum sepal length (numeric)
#* @param min_petal_length Minimum petal length (numeric)
#* @param max_petal_length Maximum petal length (numeric)
#* @param limit         Max rows to return (default: all)
#* @serializer json
function(species          = NULL,
         min_sepal_length = NULL,
         max_sepal_length = NULL,
         min_petal_length = NULL,
         max_petal_length = NULL,
         limit            = NULL,
         res) {
  
  df <- iris_data
  
  # --- Filter by species (case-insensitive) ---
  if (!is.null(species) && nzchar(species)) {
    df <- df[tolower(df$species) == tolower(species), ]
  }
  
  # --- Numeric range filters ---
  if (!is.null(min_sepal_length))
    df <- df[df$sepal_length >= as.numeric(min_sepal_length), ]
  if (!is.null(max_sepal_length))
    df <- df[df$sepal_length <= as.numeric(max_sepal_length), ]
  if (!is.null(min_petal_length))
    df <- df[df$petal_length >= as.numeric(min_petal_length), ]
  if (!is.null(max_petal_length))
    df <- df[df$petal_length <= as.numeric(max_petal_length), ]
  
  # --- Limit ---
  if (!is.null(limit)) {
    n <- as.integer(limit)
    if (!is.na(n) && n > 0) df <- head(df, n)
  }
  
  # Add a row_id column for reference
  df$row_id <- as.integer(rownames(df))
  
  list(
    total  = nrow(df),
    data   = df
  )
}


#* @get /iris/species
#* @serializer json
function() {
  list(species = sort(unique(as.character(iris_data$species))))
}


#* @get /iris/summary
#* @serializer json
function() {
  numeric_cols <- c("sepal_length", "sepal_width", "petal_length", "petal_width")
  species_list <- sort(unique(as.character(iris_data$species)))
  
  result <- lapply(species_list, function(sp) {
    sub_df <- iris_data[iris_data$species == sp, numeric_cols]
    stats  <- lapply(numeric_cols, function(col) {
      vals <- sub_df[[col]]
      list(
        mean   = round(mean(vals),   2),
        sd     = round(sd(vals),     2),
        min    = round(min(vals),    2),
        median = round(median(vals), 2),
        max    = round(max(vals),    2)
      )
    })
    names(stats) <- numeric_cols
    c(list(species = sp, count = nrow(sub_df)), stats)
  })
  
  list(summary = result)
}


#* @get /iris/<id:int>
#* @serializer json
function(id, res) {
  if (id < 1 || id > nrow(iris_data)) {
    res$status <- 404
    return(list(error = paste0("Row ", id, " not found. Valid range: 1–", nrow(iris_data))))
  }
  row        <- iris_data[id, , drop = FALSE]
  row$row_id <- id
  row
}


#* @post /iris
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)
  
  if (is.null(body)) {
    res$status <- 400
    return(list(error = "Invalid JSON body."))
  }
  
  required <- c("sepal_length", "sepal_width", "petal_length", "petal_width", "species")
  missing  <- setdiff(required, names(body))
  
  if (length(missing) > 0) {
    res$status <- 400
    return(list(
      error   = "Missing required fields.",
      missing = missing,
      required = required
    ))
  }
  

  numeric_fields <- c("sepal_length", "sepal_width", "petal_length", "petal_width")
  for (field in numeric_fields) {
    val <- suppressWarnings(as.numeric(body[[field]]))
    if (is.na(val) || val <= 0) {
      res$status <- 400
      return(list(error = paste0(field, " must be a positive number.")))
    }
  }
  

  valid_species <- c("setosa", "versicolor", "virginica")
  if (!tolower(body$species) %in% valid_species) {
    res$status <- 400
    return(list(
      error          = paste0("Invalid species '", body$species, "'."),
      valid_species  = valid_species
    ))
  }

  new_row <- data.frame(
    sepal_length = as.numeric(body$sepal_length),
    sepal_width  = as.numeric(body$sepal_width),
    petal_length = as.numeric(body$petal_length),
    petal_width  = as.numeric(body$petal_width),
    species      = tolower(body$species),
    stringsAsFactors = FALSE
  )
  
  iris_data <<- rbind(iris_data, new_row)
  new_id    <- nrow(iris_data)
  
  res$status <- 201
  list(
    message = "Row inserted successfully.",
    row_id  = new_id,
    data    = new_row
  )
}