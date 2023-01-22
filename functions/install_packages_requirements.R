## Install Packages
## using requirements.txt file

install_packages <- function(packages = NULL, path_to_requirements_file = NULL) {
  packages_file <- ""
  if (!is.null(path_to_requirements_file)) {
    con <- file(path_to_requirements_file)
    packages_file <- readLines(con = con, warn = FALSE)
    close(con)
  }
  
  # if (length(packages) == 1) {packages <- strsplit(packages, split = "\n")[[1]]}
  packages <- c(packages, packages_file)
  packages <- packages[!grepl(pattern = "^(#)", x = packages)]
  packages <- packages[nchar(packages) > 0]
  packages <- unique(packages)
  print(packages)
  
  # for (package in packages) {
  #   cat(paste0(
  #     "\n\n## Starting to install '",
  #     package,
  #     "' with all dependencies:\n"
  #   ))
  #   if (grepl(pattern = "/", x = package)) {
  #     if (grepl(pattern = "@", x = package)) {
  #       branch <- gsub(pattern = "^(.*)@",replacement = "",x = package)
  #       remotes::install_github(repo = package, ref = branch)
  #     } else {
  #       remotes::install_github(repo = package)
  #     }
  #   } else {
  #     remotes::update_packages(packages = package, build_manual = FALSE, quiet = TRUE, upgrade = "always")
  #   }
  # }
}


path_to_req.txt <- "/Users/tomazkastrun/Documents/tomaztk_github/Useless_R_functions/functions/requirements.txt"
install_packages(path_to_requirements_file = path_to_req.txt)

# Check the requirements.txt file
## ---------------------
## ggplot
## caret
## leaflet
## plotly