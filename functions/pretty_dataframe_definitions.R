create_console_dataframe_for_hardcoding <- function(dataframe,
                                                    round_significant_digits = 7,
                                                    rows_all_identical_width = T,
                                                    hard_code_factor_levels = F) {
  #' Function to help hard code test fixtures more quickly when documenting function outputs. This
  #' generates a command to define the provided dataframe and logs it in the console for quick copy-
  #' paste transfer into a test expectation.
  #'
  #' @details Limitations or possible issues: no ordering of factor levels is included (you may have to
  #'           add that to the output post hoc if all.equal comparison says "Component "levels"" differs
  #'           in some capacity.)
  #'
  #' @param dataframe                <data.frame> Dataframe to generate
  #' @param round_significant_digits <integer> How many significant digits to preserve for numeric values
  #' @param rows_all_identical_width <logical> Flag whether to use maximum character count of single longest
  #'                                   string representation as the field width throughout the whole table,
  #'                                   or whether to use the maximum count by row to save a little horizontal
  #'                                   space.
  #' @param hard_code_factor_levels  <logical> Flag whether to hard code the factor level names and order
  #'
  #' @return Nothing - dataframe generation string is logged in the console with `cat`
  #'
  #' @example We can create a sample dataframe with many classes of data (numeric, factor, character, logical)
  #'          and some missing entries (NA).
  #'           sample_dataframe = iris[c(1, 51, 79, 101),]
  #'           sample_dataframe$logic_flag = c(NA, T, F, T)
  #'           sample_dataframe$character = c("a","b","unknown","c")
  #'           sample_dataframe[2, 1] = NA
  #' # Dataframe looks like:
  #' #      Sepal.Length Sepal.Width Petal.Length Petal.Width    Species logic_flag character
  #' #  1            5.1         3.5          1.4         0.2     setosa         NA         a
  #' #  51            NA         3.2          4.7         1.4 versicolor       TRUE         b
  #' #  79           6.0         2.9          4.5         1.5 versicolor      FALSE   unknown
  #' #  101          6.3         3.3          6.0         2.5  virginica       TRUE         c
  #' # Let's run the method on it:
  #'           create_console_dataframe_for_hardcoding(sample_dataframe,
  #'                                                   rows_all_identical_width = T,
  #'                                                   hard_code_factor_levels = T)
  #' # Result in console:
  #' data.frame(Sepal.Length =        c(         5.1,           NA,            6,          6.3),
  #'            Sepal.Width  =        c(         3.5,          3.2,          2.9,          3.3),
  #'            Petal.Length =        c(         1.4,          4.7,          4.5,            6),
  #'            Petal.Width  =        c(         0.2,          1.4,          1.5,          2.5),
  #'            Species      = factor(c(    "setosa", "versicolor", "versicolor",  "virginica"),                       
  #'                                  levels = c("setosa", "versicolor", "virginica")),
  #'            logic_flag   =        c(          NA,         TRUE,        FALSE,         TRUE),
  #'            character    =        c(         "a",          "b",    "unknown",          "c"),
  #'            stringsAsFactors = FALSE,
  #'            row.names = c("1", "51", "79", "101"))
  #' # Note that the verticals (rows in finished dataframe) are sometimes wider than they need to be.
  #' # To make them as narrow as possible while preserving alignment, set `rows_all_identical_width = F`.
  #' # If factor levels do not need to be hardcoded, set `hard_code_factor_levels = F`.
  #' # For comparison:
  #'           create_console_dataframe_for_hardcoding(sample_dataframe,
  #'                                                   rows_all_identical_width = F,
  #'                                                   hard_code_factor_levels = F)
  #' # Result in console:
  #' data.frame(Sepal.Length =        c(     5.1,           NA,            6,         6.3),
  #'            Sepal.Width  =        c(     3.5,          3.2,          2.9,         3.3),
  #'            Petal.Length =        c(     1.4,          4.7,          4.5,           6),
  #'            Petal.Width  =        c(     0.2,          1.4,          1.5,         2.5),
  #'            Species      = factor(c("setosa", "versicolor", "versicolor", "virginica")),
  #'            logic_flag   =        c(      NA,         TRUE,        FALSE,        TRUE),
  #'            character    =        c(     "a",          "b",    "unknown",         "c"),
  #'            stringsAsFactors = FALSE,
  #'            row.names = c("1", "51", "79", "101"))
 
  # Column names and types
  columns = colnames(dataframe)
  column_classes = sapply(dataframe, class)
  column_name_field_size = max(nchar(columns))
  contains_factor = ("factor" %in% column_classes)
  contains_character = ("character" %in% column_classes)
  contains_integer = ("integer" %in% column_classes)
 
  # Row names
  rows = rownames(dataframe)
 
  # Data fields get rounded for numeric, then find maximum size of the field for alignment
  numeric_columns = columns[sapply(dataframe, is.numeric)]
  dataframe[numeric_columns] = signif(dataframe[numeric_columns],
                                      round_significant_digits)
 
  # Get data field widths
  data_field_size = get_data_field_size(dataframe,
                                        columns,
                                        column_classes,
                                        rows_all_identical_width)
 
  # Start building string representation
  string_representation = c("data.frame(")
 
  for (column_name in columns) {
    col_definition = sprintf(paste0("%s%",
                                    column_name_field_size - nchar(column_name),
                                    "s = %s,\n"),
                             column_name,
                             "",
                             create_column_entry_string(dataframe[[column_name]],
                                                        column_classes[[column_name]],
                                                        data_field_size,
                                                        contains_factor,
                                                        contains_integer,
                                                        contains_character,
                                                        hard_code_factor_levels))
    string_representation = paste0(string_representation,
                                   col_definition)
  }

  string_representation = paste0(string_representation,
                                 "stringsAsFactors = ",
                                 !contains_character)
 
  if (! all(rows == as.character(1:nrow(dataframe)))) {
    string_representation = paste0(string_representation,
                                   ",\nrow.names = c(",
                                   paste0('"',
                                          rows,
                                          '"',
                                          collapse = ", "),
                                   "))")
  } else {
    string_representation = paste0(string_representation,
                                   ")")
  }

  cat(string_representation)
}
 

get_data_field_size <- function(dataframe,
                                columns,
                                column_classes,
                                rows_all_identical_width) {
  #' Calculation of how many characters are needed to accommodate the largest entry in the dataframe to
  #' ensure alignment.
  #'
  #' @param dataframe                <data.frame> Dataframe to generate
  #' @param columns <character vector> Names of dataframe columns
  #' @param column_classes <character vector> Classes of dataframe columns
  #' @param rows_all_identical_width <logical> Flag whether to use maximum character count of single longest
  #'                                   string representation as the field width throughout the whole table,
  #'                                   or whether to use the maximum count by row to save a little horizontal
  #'                                   space.
  #'
  #' @return `data_field_size`, either a single integer to set the width of every table entry or a vector of
  #'         integers length matching the number of dataframe rows.
  #'        
  #' @example We can create a sample dataframe with many classes of data (numeric, factor, character, logical)
  #'          and some missing entries (NA).
  #'           sample_dataframe = iris[c(1, 51, 79, 101),]
  #'           sample_dataframe$logic_flag = c(NA, T, F, T)
  #'           sample_dataframe$character = c("a","b","unknown","c")
  #'           sample_dataframe[2, 1] = NA
  #' # Dataframe looks like:
  #' #      Sepal.Length Sepal.Width Petal.Length Petal.Width    Species logic_flag character
  #' #  1            5.1         3.5          1.4         0.2     setosa         NA         a
  #' #  51            NA         3.2          4.7         1.4 versicolor       TRUE         b
  #' #  79           6.0         2.9          4.5         1.5 versicolor      FALSE   unknown
  #' #  101          6.3         3.3          6.0         2.5  virginica       TRUE         c
  #' # Let's run the method on it:
  #'         get_data_field_size(sample_dataframe,
  #'                             names(sample_dataframe),
  #'                             c("numeric", "numeric", "numeric", "numeric", "factor", "logical", "character"),
  #'                             T)
  #' # Result: 12, which corresponds to the number of characters needed to fit the largest entry in the
  #' #             table (versicolor, which has 10 characters + 2 for quotation marks around for string
  #' #             format.)
  #' # Let's run again with the row width flag toggled to false:
  #'         get_data_field_size(sample_dataframe,
  #'                             names(sample_dataframe),
  #'                             c("numeric", "numeric", "numeric", "numeric", "factor", "logical", "character"),
  #'                             F)
  #' # Result: c(8, 12, 12 11) by row name. This is the number of characters needed to fit the largest
  #'                           entry in the table on a per-row basis. Note that rows a visualized vertically
  #'                           during dataframe definition. These numbers come from the "Species" column +
  #'                           quotation marks.
 
  # Change NA values to a string representation to avoid frustration with character count
  dataframe[is.na(dataframe)] = "NA"

  character_counts = apply(dataframe, c(1,2), function(y) nchar(y)) 

  # Add 2 extra characters for quotes around strings
  column_needs_quotes = column_classes %in% c("factor", "character")
  character_counts[,columns[column_needs_quotes]] = character_counts[,columns[column_needs_quotes]] + 2
 
  # Determine whether to return a single value or a vector with a value for each row
  if (rows_all_identical_width) {
    data_field_size = max(character_counts, na.rm = T)
  } else {
    data_field_size = apply(character_counts, 1, function(x) max(x, na.rm = T))
  }

  return(data_field_size)
}

 
create_column_entry_string <- function(column_contents,
                                       column_class,
                                       field_size,
                                       contains_factor,
                                       contains_integer,
                                       contains_character,
                                       hard_code_factor_levels) {
  #' Create a string to define a single column in the dataframe
  #'
  #' @param column_contents    <vector> Values in the column
  #' @param column_class       <character> Class of the column
  #'                              Can for sure handle numeric, integer, character, factor, logical
  #' @param field_size         <integer> Number of characters for each element, including whitespace padding.
  #'                            Can be a single value to use everywhere, or a vector to control how much
  #'                            space each element gets on a row-wise bases.
  #' @param contains_factor    <logical> Whether the dataframe as a whole contains any factor columns
  #' @param contains_integer   <logical> Whether the dataframe as a whole contains any integer columns
  #' @param contains_character <logical> Whether the dataframe as a whole contains any character columns
  #' @param hard_code_factor_levels  <logical> Flag whether to hard code the factor level names and order
  #'
  #' @return Vector definition of the column as a string, e.g. "c(10,  3, 34)", with appropriate spacing.
  #'          See examples below.
  #'
  #' @example We can create a sample dataframe with many classes of data (numeric, factor, character, logical)
  #'          and some missing entries (NA).
  #'           sample_dataframe = iris[c(1, 51, 79, 101),]
  #'           sample_dataframe$logic_flag = c(NA, T, F, T)
  #'           sample_dataframe$character = c("a","b","unknown","c")
  #'           sample_dataframe[2, 1] = NA
  #' # Dataframe looks like:
  #' #      Sepal.Length Sepal.Width Petal.Length Petal.Width    Species logic_flag character
  #' #  1            5.1         3.5          1.4         0.2     setosa         NA         a
  #' #  51            NA         3.2          4.7         1.4 versicolor       TRUE         b
  #' #  79           6.0         2.9          4.5         1.5 versicolor      FALSE   unknown
  #' #  101          6.3         3.3          6.0         2.5  virginica       TRUE         c
  #' # Let's get some columns:
  #'        create_column_entry_string(sample_dataframe$Sepal.Length,
  #'                                   "numeric",
  #'                                   field_size = 12,
  #'                                   contains_factor = T,
  #'                                   contains_integer = F,
  #'                                   contains_character = T,
  #'                                   hard_code_factor_levels = F)
  #' # Results: "       c(         5.1,           NA,            6,          6.3)"
  #' # Another column:
  #'        create_column_entry_string(sample_dataframe$Species,
  #'                                   "factor",
  #'                                   field_size = 12,
  #'                                   contains_factor = T,
  #'                                   contains_integer = F,
  #'                                   contains_character = T,
  #'                                   hard_code_factor_levels = F)
  #' # Results: "factor(c(    "setosa", "versicolor", "versicolor",  "virginica"))"
  #' # Another column:
  #'        create_column_entry_string(sample_dataframe$logic_flag,
  #'                                   "logical",
  #'                                   field_size = 12,
  #'                                   contains_factor = T,
  #'                                   contains_integer = F,
  #'                                   contains_character = T,
  #'                                   hard_code_factor_levels = F)
  #' # Results: "       c(          NA,         TRUE,        FALSE,         TRUE)"

  # Determine formatting for each entry
  if (length(field_size) == 1) {
    field_size = rep(field_size,
                     length(column_contents))
  }

  # Single or multi-row?
  contains_multiple_rows = (length(column_contents) > 1)
 
  format_setting = paste0(ifelse(contains_multiple_rows,
                                 "c(",
                                 ""),
                          paste0("%",
                                 field_size,
                                 "s",
                                 collapse = ", "),
                          ifelse(contains_multiple_rows,
                                 ")",
                                 ""))

  if (column_class %in% c("character", "factor")) {
    if (column_class == "factor") {
      level_description = paste0(",\nlevels = c(",
                                 paste0('"',
                                        levels(column_contents),
                                        '"',
                                        collapse = ", "),
                                 ")")
    }

    na_indexing = is.na(column_contents)
    column_contents = paste0('"',
                             column_contents,
                             '"')
    column_contents[na_indexing] = NA
  }

  text_definition = do.call(sprintf,
                            c(fmt = format_setting,
                              as.list(column_contents)))
 
  left_whitespace_width = max(ifelse(contains_factor & (contains_character | hard_code_factor_levels), 7, 0),
                              ifelse(contains_integer, 11, 0)) -
                              ifelse(column_class == "factor" & (contains_character | hard_code_factor_levels), 7, 0) -
                              ifelse(column_class == "integer", 11, 0)

  if (column_class == "factor" & (contains_character | hard_code_factor_levels)) {
    # Need to add factor command
    text_definition = paste0("factor(",
                             text_definition,
                             ifelse(hard_code_factor_levels,
                                    level_description,
                                    ""),
                             ")")
   
  } else if (column_class == "integer") {
    # Need to add as.integer coercion
    text_definition = paste0("as.integer(",
                             text_definition,
                             ")")   
  }

  # Need to match bracket alignment of factor/integer column
  text_definition = sprintf(paste0("%",
                                   left_whitespace_width,
                                   "s%s"),
                            "",
                            text_definition)
 
  return(text_definition)
}

 
