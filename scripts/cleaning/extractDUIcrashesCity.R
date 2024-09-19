# extracts the crashes data from TXDOT data pdfs (city level)
# Last updated: 30-05-2024

years = seq(2003, 2019)

# extract data from all files 

crashdata = lapply(years, extractAllTables)
crashdata = as.data.frame(do.call(rbind, crashdata))
crashdata[,2] = as.integer(gsub(",", "", crashdata[,2])) 

# Resolve duplicate cities with different spellings

crashdata = crashdata %>%
  mutate(
    city = tolower(city),
    city = ifelse(city == "canyon city", "canyon", city)
  )
# save dataset

write.csv(crashdata, "data/clean/texas_crashes_by_cities_2003_2019.csv")


##---------------------------------------------------------------
##                          FUNCTIONS                           -
##---------------------------------------------------------------

# trim extra whitespaces from long city names

trim_city_names = function(page) {
  i <- 2
  while (i <= length(page)) {
    if (grepl("^\\s", page[[i]])) {  # Check if element starts with whitespace
      # Combine current element with previous and next one
      replace_text = paste(trimws(page[[i-1]]), trimws(page[[i+1]]))
      page[[i]] = sub("^\\s", replace_text, page[[i]])
      # Remove current and next element
      page = page[-c(i-1, i + 1)]
    } else {
      i <- i + 1
    }
  }
  return(page)
}

# extract data from a file

extractAllTables = function(year){
  
  filename = str_c("data/raw/dui-crashes-and-fatalities/cities-and-towns/", year, ".pdf")
  
  pdf_text = strsplit(pdf_text(filename), "\n")
  
  pdf_data = lapply(pdf_text, extractTable)
  
  df = do.call(rbind, pdf_data)
  
  # extract only the last column (total crashes), exclude total row
  
  df = head(df[, c(1,ncol(df))],-1)
  
  names(df) = c("city","annual_dui_crashes")
  
  df$year = year
  
  return(df)
}

# extract data from a single page

extractTable = function(page){
  
  data = page[(grep("^City", page)+1):(grep("^(Information|Effective)", page)-1)]
  
  data = trim_city_names(data)
  
  page_data = lapply(data, function(x) unlist(strsplit(x, " {2,}")))
  
  df = as.data.frame(do.call(rbind, page_data))
  
  return(df)
  
}
