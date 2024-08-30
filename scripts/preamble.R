# preamble file that loads packages

# Last updated : 26-08-2024

# load packages

packages = c(
  "foreign", "haven", "readxl", # reading foreign files
  "dplyr", "tidyr", "readr", "purrr",   # data cleaning
  "lubridate", # dates manipulation
  "stringr", # string manipulation
  "pdftools", "tesseract", "rvest", # scraping data from wedpage/pdf
  "httr", "jsonlite", # downloading data through API
  "network", "igraph", "ggnetwork", # network diagrams
  "modelsummary",  # regression tables
  "sf",         # reading spatial data
  "ggplot2", "patchwork", "paletteer",   # plotting
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
startYear = 1990 # starting year of Panel
endYear = 2019 # Ending year of panel
years = seq(startYear, endYear)

startQuarter = 1 # 1st quarter of 1990
endQuarter = (endYear-startYear)*4
quarters = seq(startQuarter, endQuarter)

# function to convert a date to a quarter
convert_to_quarter <- function(date) {
  
  year = year(date)
  month = month(date)
  
  quarter = ceiling(month / 3)
  total_quarters = (max(year,startYear) - startYear) * 4 + quarter
  
  return(total_quarters)
}

# Add mean of dependent variable to fixest

glance_custom.fixest <- function(x, ...) {
  data.table::data.table("Mean(DV)" = sprintf("%.3f",as.numeric(fitstat(x, type = "my"))))
}
