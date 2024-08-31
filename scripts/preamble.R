# preamble file that loads packages

# Last updated : 06-07-2024

# load packages

packages = c(
  "foreign", "haven", "readxl", # reading foreign files
  "dplyr", "tidyr", "readr", "purrr",   # data cleaning
  "lubridate", # dates
  "stringr", 
  "pdftools", "tesseract", "rvest", # scraping data
  "httr", "jsonlite", # downloading data through API
  "modelsummary",  # regression tables
  "sf",         # reading spatial data
  "ggplot2", "patchwork", "RColorBrewer",   # plotting
  "ggalluvial",
  # Econometric packages
  "did", # Calloway-Sant'anna
  "bacondecomp", # Bacon Decomposition
  "fixest" #TWFE
)

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}


##----------------------------------------------------------------
##                        CUSTOM FUNCTIONS                       -
##----------------------------------------------------------------

# clean string numbers, removing commas

clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.]", "", x))
}
